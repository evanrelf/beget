{-# LANGUAGE QuasiQuotes #-}

module Beget.Trace
  ( Trace (..)
  , Database
  , withDatabase
  , withConnection
  , dbDrop
  , dbCreate
  , fetchTraces
  , insertTrace
  )
where

import Beget.Hash (BegetHash (..), begetHash)
import Beget.Value (Value)
import Codec.Serialise (Serialise, deserialise, serialise)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (assert, onException)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString.Lazy (LazyByteString)
import Data.Foldable (for_)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int64)
import Data.List (groupBy)
import Data.String.Interpolate (iii)
import Data.Traversable (for)
import Database.SQLite.Simple qualified as SQLite
import GHC.Generics (Generic)

data Trace k v = Trace
  { key :: k
  , deps :: [HashMap k BegetHash]
  , value :: v
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (Serialise)

newtype Database = Database (MVar SQLite.Connection)

withDatabase :: FilePath -> (Database -> IO a) -> IO a
withDatabase path action =
  SQLite.withConnection path \connection -> do
    mvar <- newMVar connection
    action (Database mvar)

withConnection :: MonadIO m => Database -> (SQLite.Connection -> IO a) -> m a
withConnection (Database mvar) action = liftIO $ withMVar mvar action

dbDrop :: MonadIO m => Database -> m ()
dbDrop database = withConnection database \connection ->
  SQLite.withTransaction connection do
    SQLite.execute_ connection "drop table if exists traces"
    SQLite.execute_ connection "drop table if exists trace_deps"

dbCreate :: MonadIO m => Database -> m ()
dbCreate database = withConnection database \connection ->
  SQLite.withTransaction connection do
    SQLite.execute_ connection [iii|
      create table traces (
        id integer primary key,
        key blob not null,
        value blob not null,
        trace_hash blob not null unique
      ) strict
    |]
    SQLite.execute_ connection [iii|
      create table trace_deps (
        trace_id integer not null references traces on delete cascade,
        cohort integer not null,
        dep_key blob not null,
        dep_value_hash blob not null,
        unique (trace_id, dep_key)
      ) strict
    |]
    SQLite.execute_ connection [iii|
      create index idx_traces_key on traces(key)
    |]
    SQLite.execute_ connection [iii|
      create trigger forbid_trace_update
      before update on traces
      begin
        select raise(abort, 'traces are immutable');
      end
    |]
    SQLite.execute_ connection [iii|
      create trigger forbid_trace_deps_update
      before update on trace_deps
      begin
        select raise(abort, 'trace dependencies are immutable');
      end
    |]
    SQLite.execute_ connection [iii|
      create trigger forbid_trace_deps_delete
      before delete on trace_deps
      when (select count(*) from traces where id = old.trace_id) > 0
      begin
        select raise(abort, 'trace dependencies cannot be deleted directly');
      end
    |]

fetchTraces
  :: (Value k, Value v, MonadIO m)
  => Database -> Maybe k -> m [Trace k v]
fetchTraces database mKey = withConnection database \connection -> do
  traceRows :: [(Int64, LazyByteString, LazyByteString, LazyByteString)] <-
    case mKey of
      Just key -> do
        let keyBytes = serialise key
        SQLite.query
          connection
          "select id, key, value, trace_hash from traces where key = ?"
          (SQLite.Only keyBytes)
      Nothing ->
        SQLite.query_
          connection
          "select id, key, value, trace_hash from traces"

  for traceRows \traceRow -> do
    let (traceId, traceKeyBytes, traceValueBytes, traceHashBytes) = traceRow

    let traceKey = deserialise traceKeyBytes

    case mKey of
      Just key -> assert (key == traceKey) $ pure ()
      Nothing -> pure ()

    let traceValue = deserialise traceValueBytes

    depsRows :: [(Int, LazyByteString, LazyByteString)] <-
      SQLite.query
        connection
        "select cohort, dep_key, dep_value_hash from trace_deps where trace_id = ? order by cohort"
        (SQLite.Only traceId)

    deps :: [(Int, (k, BegetHash))] <-
      for depsRows \(cohort, depKeyBytes, depValueHashBytes) -> do
        let depKey = deserialise depKeyBytes
        let depValueHash = deserialise depValueHashBytes
        pure (cohort, (depKey, depValueHash))

    let traceHash = deserialise traceHashBytes

    let trace = Trace
          { key = traceKey
          , deps = map (HashMap.fromList . map snd) (groupBy ((==) `on` fst) deps)
          , value = traceValue
          }

    assert (begetHash trace == traceHash) $ pure ()

    pure trace

insertTrace :: (Value k, Value v, MonadIO m) => Database -> Trace k v -> m Int64
insertTrace database trace = withConnection database \connection -> do
  flip onException (SQLite.execute_ connection "rollback") do
    SQLite.execute_ connection "begin"

    let keyBytes = serialise trace.key
    let valueBytes = serialise trace.value
    let traceHashBytes = serialise (begetHash trace)

    SQLite.execute
      connection
      "insert or ignore into traces (key, value, trace_hash) values (?, ?, ?)"
      (keyBytes, valueBytes, traceHashBytes)

    rows :: [(Int64, Bool)] <-
      SQLite.query
        connection
        "select id, changes() == 0 as is_dupe from traces where trace_hash = ?"
        (SQLite.Only traceHashBytes)

    (traceId, isDupe) <-
      case rows of
        [row] -> pure row
        [] -> error "No rows"
        _ : _ : _ -> error "More than one row"

    if isDupe then do
      putStrLn $ "warn: Trace " <> show traceId <> " already exists in database"
      SQLite.execute_ connection "rollback"
      pure traceId

    else do
      for_ (zip [0 :: Int ..] trace.deps) \(cohort, deps) ->
        for_ (HashMap.toList deps) \(depKey, depValueHash) -> do
          let depKeyBytes = serialise depKey
          let depValueHashBytes = serialise depValueHash

          SQLite.execute
            connection
            "insert into trace_deps (trace_id, cohort, dep_key, dep_value_hash) values (?, ?, ?, ?)"
            (traceId, cohort, depKeyBytes, depValueHashBytes)

      SQLite.execute_ connection "commit"

      pure traceId
