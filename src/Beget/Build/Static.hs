{-# LANGUAGE MultiWayIf #-}

module Beget.Build.Static
  ( BuildState (..)
  , newBuildState
  , TaskState (..)
  , newTaskState
  , taskStateRealize
  )
where

import Beget.Hash (BegetHash, begetHash)
import Beget.Trace (Trace (..), fetchTraces, insertTrace)
import Beget.Value (Value)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (SomeException, assert)
import Control.Exception.Annotated.UnliftIO qualified as Exception
import Control.Monad (filterM, unless, void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Maybe (fromMaybe, isNothing)
import Database.SQLite.Simple qualified as SQLite
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (forConcurrently_, race_)

data BuildState k v = BuildState
  { tasks :: TaskState k v -> k -> IO (v, Bool)
  , connection :: SQLite.Connection
  , done :: TVar (HashMap k (TMVar (Either SomeException ())))
  , store :: TVar (HashMap k v)
  , debugTaskCount :: TVar Int
  }

newBuildState
  :: SQLite.Connection
  -> (TaskState k v -> k -> IO (v, Bool))
  -> STM (BuildState k v)
newBuildState connection tasks = do
  done <- newTVar HashMap.empty
  store <- newTVar HashMap.empty
  debugTaskCount <- newTVar 0
  pure BuildState{ tasks, connection, done, store, debugTaskCount }

buildStateRealize :: (Value k, Value v) => BuildState k v -> k -> IO v
buildStateRealize buildState key = do
  eBarrier <- atomically do
    done <- readTVar buildState.done
    case HashMap.lookup key done of
      Just barrier -> pure (Right barrier)
      Nothing -> do
        barrier <- newEmptyTMVar
        modifyTVar' buildState.done (HashMap.insert key barrier)
        pure (Left barrier)

  case eBarrier of
    Right barrier -> do
      result <- atomically $
        readTMVar barrier >>= \case
          Right () -> do
            store <- readTVar buildState.store
            -- SAFETY: The barrier is filled only after the value is inserted into store.
            -- Both operations happen in the same STM transaction in the builder.
            let value = fromMaybe (error "unreachable") (HashMap.lookup key store)
            pure (Right value)
          Left exception -> pure (Left exception)

      case result of
        Right value -> pure value
        Left exception -> Exception.throw exception

    Left barrier -> do
      eValue <- Exception.try @SomeException do
        buildStateFetch buildState key >>= \case
          Just value -> pure value
          Nothing -> do
            atomically $ modifyTVar' buildState.debugTaskCount (+ 1)
            buildStateBuild buildState key

      case eValue of
        Right value -> atomically do
          modifyTVar' buildState.store (HashMap.insert key value)
          -- SAFETY: The key has not been marked as done yet, and no other tasks
          -- will attempt to.
          putTMVar barrier (Right ())
          pure value
        Left exception -> do
          -- SAFETY: The key has not been marked as done yet, and no other tasks
          -- will attempt to.
          atomically $ putTMVar barrier (Left exception)
          Exception.throw exception

buildStateFetch :: (Value k, Value v) => BuildState k v -> k -> IO (Maybe v)
buildStateFetch buildState key = do
  traces <- fetchTraces buildState.connection (Just key)

  matches :: HashSet v <-
    HashSet.fromList . map (.value) <$> do
      traces & filterM \trace -> do
        assert (begetHash trace.key == begetHash key) $ pure ()
        HashMap.toList trace.deps & allConcurrently \(depKey, depValueHash) -> do
          -- TODO: Wrap possible exception from failure to realize in checkpoint
          -- indicating this unexpectedly happened during fetch.
          depValue <- buildStateRealize buildState depKey
          pure (depValueHash == begetHash depValue)

  store <- readTVarIO buildState.store

  if| Just storeValue <- HashMap.lookup key store
    , HashSet.member storeValue matches ->
        pure (Just storeValue)
    | cachedValue : _ <- HashSet.toList matches ->
        pure (Just cachedValue)
    | otherwise ->
        pure Nothing

buildStateBuild :: (Value k, Value v) => BuildState k v -> k -> IO v
buildStateBuild buildState key = do
  taskState <- atomically $ newTaskState buildState
  (value, volatile) <- buildState.tasks taskState key
  when (not volatile) do
    deps <- readTVarIO taskState.deps
    _traceId <- insertTrace buildState.connection Trace{ key, deps, value }
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

allConcurrently
  :: (Foldable f, MonadUnliftIO m)
  => (a -> m Bool) -> f a -> m Bool
allConcurrently f xs = do
  m <- liftIO newEmptyMVar
  forConcurrently_ xs \x ->
    race_
      (unlessM (f x) (liftIO $ void (tryPutMVar m ())))
      (liftIO $ readMVar m)
  liftIO $ isNothing <$> tryReadMVar m
  where
  unlessM p m = p >>= flip unless m
