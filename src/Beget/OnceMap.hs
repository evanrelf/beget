module Beget.OnceMap
  ( OnceMap
  , new
  , getOrRun
  , toHashMap
  , Result (..)
  )
where

import Control.Concurrent.STM
import Control.Exception (SomeAsyncException (..), SomeException)
import Control.Exception qualified as Exception
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)

data Result v
  = Failed SomeException
  | Cancelled
  | Succeeded v
  deriving stock (Show)

newtype OnceMap k v = OnceMap (TVar (HashMap k (TMVar (Result v))))

new :: STM (OnceMap k v)
new = OnceMap <$> newTVar HashMap.empty

getOrRun :: Hashable k => OnceMap k v -> k -> IO v -> IO v
getOrRun (OnceMap table) key action = Exception.mask \restore -> do
  claim <- atomically do
    barriers <- readTVar table
    case HashMap.lookup key barriers of
      Just barrier -> pure $ Left barrier
      Nothing -> do
        barrier <- newEmptyTMVar
        writeTVar table (HashMap.insert key barrier barriers)
        pure $ Right barrier

  case claim of
    Left barrier -> restore do
      atomically (readTMVar barrier) >>= \case
        Succeeded value -> pure value
        Failed exception -> Exception.throwIO exception
        Cancelled -> getOrRun (OnceMap table) key action

    Right barrier -> do
      result <- Exception.try @SomeException (restore action)
      atomically $ case result of
        Right value -> putTMVar barrier (Succeeded value)
        Left exception | isAsyncException exception -> do
          putTMVar barrier Cancelled
          modifyTVar' table (HashMap.delete key)
        Left exception -> putTMVar barrier (Failed exception)
      either Exception.throwIO pure result

toHashMap :: OnceMap k v -> STM (HashMap k (Maybe (Result v)))
toHashMap (OnceMap table) = readTVar table >>= traverse tryReadTMVar

isAsyncException :: SomeException -> Bool
isAsyncException exception =
  case Exception.fromException exception of
    Just (SomeAsyncException _) -> True
    Nothing -> False
