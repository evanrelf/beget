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
import Control.Monad (unless, void, when)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List (sortOn)
import Data.Maybe (isNothing)

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
  match <-
    traces & findM \trace -> do
      Exception.assert (begetHash trace.key == begetHash key) $ pure ()
      HashMap.toList (HashMap.unions trace.deps) & allConcurrently \(depKey, depValueHash) -> do
        -- TODO: Wrap possible exception from failure to realize in checkpoint
        -- indicating this unexpectedly happened during fetch.
        depValue <- buildStateRealize buildState depKey
        pure (depValueHash == begetHash depValue)
  pure $ fmap (.value) match

buildStateBuild :: (Value k, Value v) => BuildState k v -> k -> IO v
buildStateBuild buildState key = do
  taskState <- atomically $ newTaskState buildState
  (value, volatile) <- buildState.tasks taskState key
  depFailed <- readTVarIO taskState.depFailed
  when (not volatile && not depFailed) do
    depsByCohort <- readTVarIO taskState.deps
    let deps =
          HashMap.toList depsByCohort
            & map (\(depKey, (cohort, depValueHash)) -> (cohort, HashMap.singleton depKey depValueHash))
            & HashMap.fromListWith HashMap.union
            & HashMap.toList
            & sortOn fst
            & map snd
    _traceId <- insertTrace buildState.database Trace{ key, deps, value }
    pure ()
  pure value

data TaskState k v = TaskState
  { buildState :: BuildState k v
  , deps :: TVar (HashMap k (Int, BegetHash))
  , depFailed :: TVar Bool
  }

newTaskState :: BuildState k v -> STM (TaskState k v)
newTaskState buildState = do
  deps <- newTVar HashMap.empty
  depFailed <- newTVar False
  pure TaskState{ buildState, deps, depFailed }

taskStateRealize :: (Value k, Value v) => TaskState k v -> k -> IO v
taskStateRealize taskState key =
  flip Exception.onException (atomically $ writeTVar taskState.depFailed True) do
    cohort <- HashMap.size <$> readTVarIO taskState.deps
    value <- buildStateRealize taskState.buildState key
    hash <- Exception.evaluate (begetHash value)
    let keepEarliest (cohort1, _) (cohort2, _) = (min cohort1 cohort2, hash)
    atomically $ modifyTVar' taskState.deps $ HashMap.insertWith keepEarliest key (cohort, hash)
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

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = pure Nothing
findM p (x : xs) = p x >>= \case
  True -> pure (Just x)
  False -> findM p xs
