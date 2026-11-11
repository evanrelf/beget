{-# LANGUAGE QuasiQuotes #-}

module Beget.Database
  ( Database
  , withDatabase
  , withConnection
  , dbDrop
  , dbCreate
  )
where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad.IO.Class (MonadIO (..))
import Data.String.Interpolate (iii)
import Database.SQLite.Simple qualified as SQLite

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
