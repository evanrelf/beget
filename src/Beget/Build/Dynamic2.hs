{-# LANGUAGE TypeFamilies #-}

module Beget.Build.Dynamic2
  ( Build
  , runBuild
  , Task
  , task
  , someTask
  , TaskFn
  )
where

import Beget.Build.Static qualified as Static
import Beget.Value (Value)
import Codec.Serialise (Serialise, deserialise, serialise)
import Control.Concurrent.STM (atomically)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Reader (ReaderT (..), ask, local)
import Data.ByteString (ByteString, fromStrict, toStrict)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Database.SQLite.Simple qualified as SQLite
import GHC.Generics (Generic)
import Language.Haskell.TH qualified as TH

-- Fully qualified name, e.g. `Foo.Bar.baz`
newtype TaskName = TaskName String
  deriving stock (Generic, Show, Eq)
  deriving newtype (Serialise, Hashable)

data TaskKey = TaskKey
  { name :: TaskName
  , args :: ByteString
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Serialise, Hashable, Value)

newtype TaskValue = TaskValue ByteString
  deriving stock (Generic, Show, Eq)
  deriving newtype (Serialise, Hashable)
  deriving anyclass (Value)

type TaskState = Static.TaskState TaskKey TaskValue

data Env = Env
  { taskState :: TaskState
    -- The key whose handler is running. Calling `task` with this key runs its
    -- body instead of realizing it (which would deadlock).
  , building :: Maybe TaskKey
  }

newtype Build a = Build (ReaderT Env IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

runBuild :: [Task] -> SQLite.Connection -> Build a -> IO a
runBuild tasks connection (Build m) = do
  let handlers = HashMap.fromList [(t.name, t.handler) | t <- tasks]
  let runTask taskState key =
        case HashMap.lookup key.name handlers of
          Nothing -> error $ "No handler for task `" <> show key.name <> "`"
          Just handler -> do
            let Build action = handler key.args
            bytes <- runReaderT action Env{ taskState, building = Just key }
            pure (TaskValue bytes, False)
  buildState <- atomically $ Static.newBuildState connection runTask
  taskState <- atomically $ Static.newTaskState buildState
  runReaderT m Env{ taskState, building = Nothing }

data Task = Task
  { name :: TaskName
  , handler :: ByteString -> Build ByteString
  }

task :: TaskFn f => TH.Name -> f -> f
task thName f = collectArgs \args -> Build do
  env <- ask
  let key = TaskKey{ name = TaskName (show thName), args = toStrict (serialise args) }
  if env.building == Just key then do
    let Build action = applyArgs f args
    local (\env' -> env'{ building = Nothing }) action
  else do
    TaskValue bytes <- liftIO $ Static.taskStateRealize env.taskState key
    pure $ deserialise (fromStrict bytes)

someTask :: TaskFn f => TH.Name -> f -> Task
someTask thName f =
  Task
    { name = TaskName (show thName)
    , handler = \bytes ->
        toStrict . serialise <$> applyArgs f (deserialise (fromStrict bytes))
    }

class (Serialise (TaskArgs f), Serialise (TaskResult f)) => TaskFn f where
  type TaskArgs f :: Type
  type TaskResult f :: Type
  applyArgs :: f -> TaskArgs f -> Build (TaskResult f)
  collectArgs :: (TaskArgs f -> Build (TaskResult f)) -> f

instance Serialise r => TaskFn (Build r) where
  type TaskArgs (Build r) = ()
  type TaskResult (Build r) = r
  applyArgs f () = f
  collectArgs k = k ()

instance (Serialise a, TaskFn f) => TaskFn (a -> f) where
  type TaskArgs (a -> f) = (a, TaskArgs f)
  type TaskResult (a -> f) = TaskResult f
  applyArgs f (a, rest) = applyArgs (f a) rest
  collectArgs k = \a -> collectArgs \rest -> k (a, rest)
