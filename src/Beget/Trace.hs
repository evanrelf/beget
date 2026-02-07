{-# LANGUAGE QuasiQuotes #-}

module Beget.Trace
  ( Trace (..)
  , dbDrop
  , dbCreate
  , fetchTraces
  , insertTrace
  )
where

import Beget.Hash (BegetHash (..), begetHash)
import Beget.Value (Value)
import Codec.Serialise (Serialise, deserialise, serialise)
import Control.Exception (assert, onException)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString.Lazy (LazyByteString)
import Data.Foldable (for_)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int64)
import Data.String.Interpolate (iii)
import Data.Traversable (for)
import Database.SQLite.Simple qualified as SQLite
import GHC.Generics (Generic)

data Trace k v = Trace
  { key :: k
  , deps :: HashMap k BegetHash
  , value :: v
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (Serialise)

dbDrop :: MonadIO m => SQLite.Connection -> m ()
dbDrop connection = liftIO $ SQLite.withTransaction connection do
  SQLite.execute_ connection "drop table if exists traces"
  SQLite.execute_ connection "drop table if exists trace_deps"

dbCreate :: MonadIO m => SQLite.Connection -> m ()
dbCreate connection = liftIO $ SQLite.withTransaction connection do
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
  => SQLite.Connection -> Maybe k -> m [Trace k v]
fetchTraces connection mKey = liftIO do
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

    depsRows :: [(LazyByteString, LazyByteString)] <-
      SQLite.query
        connection
        "select dep_key, dep_value_hash from trace_deps where trace_id = ?"
        (SQLite.Only traceId)

    deps :: [(k, BegetHash)] <-
      for depsRows \(depKeyBytes, depValueHashBytes) -> do
        let depKey = deserialise depKeyBytes
        let depValueHash = deserialise depValueHashBytes
        pure (depKey, depValueHash)

    let traceHash = deserialise traceHashBytes

    let trace = Trace
          { key = traceKey
          , deps = HashMap.fromList deps
          , value = traceValue
          }

    assert (begetHash trace == traceHash) $ pure ()

    pure trace

insertTrace :: (Value k, Value v, MonadIO m) => SQLite.Connection -> Trace k v -> m Int64
insertTrace connection trace = liftIO do
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
      for_ (HashMap.toList trace.deps) \(depKey, depValueHash) -> do
        let depKeyBytes = serialise depKey
        let depValueHashBytes = serialise depValueHash

        SQLite.execute
          connection
          "insert into trace_deps values (?, ?, ?)"
          (traceId, depKeyBytes, depValueHashBytes)

      SQLite.execute_ connection "commit"

      pure traceId
