module Beget.Build.Static
  ( BuildState (..)
  , newBuildState
  , TaskState (..)
  , newTaskState
  , taskStateRealize
  )
where

import Beget.Hash (BegetHash, begetHash)
import Beget.OnceMap (OnceMap)
import Beget.OnceMap qualified as OnceMap
import Beget.Trace (Database, Trace (..), fetchTraces, insertTrace)
import Beget.Value (Value)
import Control.Concurrent.Async (forConcurrently_, race_)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception qualified as Exception
import Control.Monad (filterM, unless, void, when)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (isNothing, listToMaybe)

data BuildState k v = BuildState
  { tasks :: TaskState k v -> k -> IO (v, Bool)
  , database :: Database
  , store :: OnceMap k v
  , debugTaskCount :: TVar Int
  }

newBuildState
  :: Database
  -> (TaskState k v -> k -> IO (v, Bool))
  -> STM (BuildState k v)
newBuildState database tasks = do
  store <- OnceMap.new
  debugTaskCount <- newTVar 0
  pure BuildState{ tasks, database, store, debugTaskCount }

buildStateRealize :: (Value k, Value v) => BuildState k v -> k -> IO v
buildStateRealize buildState key =
  OnceMap.getOrRun buildState.store key do
    buildStateFetch buildState key >>= \case
      Just value -> pure value
      Nothing -> do
        atomically $ modifyTVar' buildState.debugTaskCount (+ 1)
        buildStateBuild buildState key

buildStateFetch :: (Value k, Value v) => BuildState k v -> k -> IO (Maybe v)
buildStateFetch buildState key = do
  traces <- fetchTraces buildState.database (Just key)
  matches <-
    traces & filterM \trace -> do
      Exception.assert (begetHash trace.key == begetHash key) $ pure ()
      HashMap.toList trace.deps & allConcurrently \(depKey, depValueHash) -> do
        -- TODO: Wrap possible exception from failure to realize in checkpoint
        -- indicating this unexpectedly happened during fetch.
        depValue <- buildStateRealize buildState depKey
        pure (depValueHash == begetHash depValue)
  pure $ listToMaybe (map (.value) matches)

buildStateBuild :: (Value k, Value v) => BuildState k v -> k -> IO v
buildStateBuild buildState key = do
  taskState <- atomically $ newTaskState buildState
  (value, volatile) <- buildState.tasks taskState key
  when (not volatile) do
    deps <- readTVarIO taskState.deps
    _traceId <- insertTrace buildState.database Trace{ key, deps, value }
    pure ()
  pure value

data TaskState k v = TaskState
  { buildState :: BuildState k v
  , deps :: TVar (HashMap k BegetHash)
  }

newTaskState :: BuildState k v -> STM (TaskState k v)
newTaskState buildState = do
  deps <- newTVar HashMap.empty
  pure TaskState{ buildState, deps }

taskStateRealize :: (Value k, Value v) => TaskState k v -> k -> IO v
taskStateRealize taskState key = do
  value <- buildStateRealize taskState.buildState key
  atomically $ modifyTVar' taskState.deps $ HashMap.insert key (begetHash value)
  pure value

allConcurrently :: Foldable f => (a -> IO Bool) -> f a -> IO Bool
allConcurrently f xs = do
  m <- newEmptyMVar
  forConcurrently_ xs \x ->
    race_
      (unlessM (f x) (void (tryPutMVar m ())))
      (readMVar m)
  isNothing <$> tryReadMVar m
  where
  unlessM p m = p >>= flip unless m
