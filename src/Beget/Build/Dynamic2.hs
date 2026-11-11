{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Beget.Build.Dynamic2
  ( Build
  , runBuild
  , Task
  , task
  , someTask
  , TaskFn
  , IsTask
  , registerTask
  , discoverTasks
  )
where

import Beget.Build.Static qualified as Static
import Beget.Registry (discoverInstances)
import Beget.Trace (Database)
import Beget.Value (Value)
import Codec.Serialise (Serialise (..), deserialise, serialise)
import Control.Concurrent.STM (atomically)
import Control.Exception (Exception)
import Control.Exception qualified as Exception
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask, local)
import Data.ByteString (ByteString, fromStrict, toStrict)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable (..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy)
import Data.Traversable (for)
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack, SrcLoc, callStack, getCallStack, prettySrcLoc)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import SomeDictOf (SomeDict, SomeDictOf (..))

newtype TaskName = TaskName TH.Name
  deriving stock (Show, Eq)

instance Hashable TaskName where
  hashWithSalt salt (TaskName name) = hashWithSalt salt (show name)

instance Serialise TaskName where
  encode (TaskName name) =
    encode (TH.namePackage name, TH.nameModule name, TH.nameBase name)
  decode = do
    (Just package, Just module_, base) <- decode
    pure $ TaskName (TH.mkNameG_v package module_ base)

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
  , callSites :: HashMap TaskName (Maybe SrcLoc)
    -- Set while `runBuild` checks each registered task. Calling `task` throws
    -- the name and call site it was given instead of running anything.
  , probing :: Bool
    -- The key whose handler is running. Calling `task` with this key runs its
    -- body instead of realizing it (which would deadlock).
  , building :: Maybe TaskKey
    -- Set when the task does arbitrary IO, so its result isn't cached.
  , volatile :: IORef Bool
  }

newtype Build a = Build (ReaderT Env IO a)
  deriving newtype (Functor, Applicative, Monad)

instance MonadIO Build where
  liftIO io = Build do
    env <- ask
    liftIO $ writeIORef env.volatile True
    liftIO io

instance MonadUnliftIO Build where
  withRunInIO inner = Build do
    env <- ask
    liftIO $ writeIORef env.volatile True
    withRunInIO \run -> inner \(Build m) -> run m

runBuild :: [Task] -> Database -> Build a -> IO a
runBuild tasks database (Build m) = do
  volatile0 <- newIORef False
  let probeEnv =
        Env
          { taskState = error "unreachable: probes stop before realizing anything"
          , callSites = HashMap.empty
          , probing = True
          , building = Nothing
          , volatile = volatile0
          }
  callSites <- fmap HashMap.fromList $ for tasks \t -> do
    let TaskName expected = t.name
    let Build probe = t.probe
    Exception.try (runReaderT probe probeEnv) >>= \case
      Left (Probed actual callSite) | actual == expected -> pure (t.name, callSite)
      Left (Probed actual _) ->
        error $ "Task `" <> show expected <> "` is defined with `task '" <> TH.nameBase actual <> "` instead of `task '" <> TH.nameBase expected <> "`"
      Right void -> absurd void
  let handlers = HashMap.fromList [(t.name, t.handler) | t <- tasks]
  let runTask taskState key =
        case HashMap.lookup key.name handlers of
          Nothing -> do
            let TaskName name = key.name
            error $ "A cached trace depends on task `" <> show name <> "`, but it's missing from the tasks given to `runBuild`"
          Just handler -> do
            let Build action = handler key.args
            volatile <- newIORef False
            let env = Env
                  { taskState
                  , callSites
                  , probing = False
                  , building = Just key
                  , volatile
                  }
            bytes <- runReaderT action env
            isVolatile <- readIORef volatile
            pure (TaskValue bytes, isVolatile)
  buildState <- atomically $ Static.newBuildState database runTask
  taskState <- atomically $ Static.newTaskState buildState
  let env = Env
        { taskState
        , callSites
        , probing = False
        , building = Nothing
        , volatile = volatile0
        }
  runReaderT m env

data Task = Task
  { name :: TaskName
  , handler :: ByteString -> Build ByteString
  , probe :: Build Void
  }

data Probed = Probed TH.Name (Maybe SrcLoc)
  deriving stock (Show)
  deriving anyclass (Exception)

task :: (HasCallStack, TaskFn f) => TH.Name -> f -> f
task thName f = collectArgs \args -> Build do
  env <- ask
  let callSite = snd <$> listToMaybe (getCallStack callStack)
  when env.probing do
    liftIO $ Exception.throwIO (Probed thName callSite)
  let key = TaskKey{ name = TaskName thName, args = toStrict (serialise args) }
  if env.building == Just key then do
    let Build action = applyArgs f args
    local (\env' -> env'{ building = Nothing }) action
  else do
    case HashMap.lookup key.name env.callSites of
      Nothing ->
        error $ "Task `" <> show thName <> "` is missing from the tasks given to `runBuild` (did you forget `registerTask '" <> TH.nameBase thName <> "`?)"
      Just registeredCallSite | registeredCallSite /= callSite -> do
        let pretty = maybe "unknown location" prettySrcLoc
        error $ "`task '" <> TH.nameBase thName <> "` at " <> pretty callSite <> " reuses the name of the registered task at " <> pretty registeredCallSite
      Just _ -> pure ()
    TaskValue bytes <- liftIO $ Static.taskStateRealize env.taskState key
    pure $ deserialise (fromStrict bytes)

someTask :: TaskFn f => TH.Name -> f -> Task
someTask thName f =
  Task
    { name = TaskName thName
    , handler = \bytes ->
        toStrict . serialise <$> applyArgs f (deserialise (fromStrict bytes))
    , probe = do
        let base = TH.nameBase thName
        let notATask :: a
            notATask = error $ "Task `" <> show thName <> "` must be defined like `" <> base <> " = task '" <> base <> " ...`"
        _ <- applyArgs f notATask
        notATask
    }

class IsTask (a :: Type) where
  reflectTask :: Proxy a -> Task

registerTask :: TH.Name -> TH.Q [TH.Dec]
registerTask thName = do
  let typeName = TH.mkName ("Task_" <> TH.nameBase thName)
  instanceDecs <-
    [d|
      instance IsTask $(TH.conT typeName) where
        reflectTask _ = someTask $(TH.liftData thName) $(TH.varE thName)
      |]
  pure $ TH.DataD [] typeName [] Nothing [] [] : instanceDecs

discoverTasks :: TH.Code TH.Q [Task]
discoverTasks =
  [||
    map
      (\(SomeDictOf proxy) -> reflectTask proxy)
      ($$discoverInstances :: [SomeDict IsTask])
  ||]

class (Serialise (TaskArgs f), Serialise (TaskResult f)) => TaskFn f where
  type TaskArgs f :: Type
  type TaskResult f :: Type
  applyArgs :: f -> TaskArgs f -> Build (TaskResult f)
  collectArgs :: (TaskArgs f -> Build (TaskResult f)) -> f

instance Serialise r => TaskFn (Build r) where
  type TaskArgs (Build r) = ()
  type TaskResult (Build r) = r
  applyArgs f ~() = f
  collectArgs k = k ()

instance (Serialise a, TaskFn f) => TaskFn (a -> f) where
  type TaskArgs (a -> f) = (a, TaskArgs f)
  type TaskResult (a -> f) = TaskResult f
  applyArgs f ~(a, rest) = applyArgs (f a) rest
  collectArgs k = \a -> collectArgs \rest -> k (a, rest)
