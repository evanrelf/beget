module Beget.Build.Dynamic2
  ( Build
  , runBuild
  , Task
  )
where

import Beget.Build.Static qualified as Static
import Beget.Value (Value)
import Codec.Serialise (Serialise)
import Control.Concurrent.STM (atomically)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.ByteString (ByteString)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Database.SQLite.Simple qualified as SQLite
import GHC.Generics (Generic)

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

newtype Build a = Build (ReaderT TaskState IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

runBuild :: [Task] -> SQLite.Connection -> Build a -> IO a
runBuild tasks connection (Build m) = do
  let handlers = HashMap.fromList [(task.name, task.handler) | task <- tasks]
  let runTask taskState key =
        case HashMap.lookup key.name handlers of
          Nothing -> error $ "No handler for task `" <> show key.name <> "`"
          Just handler -> do
            let Build action = handler key.args
            bytes <- runReaderT action taskState
            pure (TaskValue bytes, False)
  buildState <- atomically $ Static.newBuildState connection runTask
  taskState <- atomically $ Static.newTaskState buildState
  runReaderT m taskState

data Task = Task
  { name :: TaskName
  , handler :: ByteString -> Build ByteString
  }
