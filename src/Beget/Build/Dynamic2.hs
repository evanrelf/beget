module Beget.Build.Dynamic2
  ( Build
  , runBuild
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

runBuild :: SQLite.Connection -> Build a -> IO a
runBuild connection (Build m) = do
  let tasks _taskState key = error $ "No handler for task `" <> show key.name <> "`"
  buildState <- atomically $ Static.newBuildState connection tasks
  taskState <- atomically $ Static.newTaskState buildState
  runReaderT m taskState
