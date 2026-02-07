{-# OPTIONS_GHC -Wno-orphans #-}

module Beget.TraceTest where

import Beget.Hash (BegetHash (..))
import Beget.Trace
import Beget.Value (Value)
import Data.HashMap.Strict qualified as HashMap
import Database.SQLite.Simple qualified as SQLite
import Prelude hiding (trace)
import Test.Tasty.HUnit

instance Value Text

unit_trace_roundtrip :: Assertion
unit_trace_roundtrip =
  SQLite.withConnection ":memory:" \connection -> do
    dbDrop connection
    dbCreate connection

    let key = "password"
    let deps = HashMap.singleton "answer" (BegetHash 42)
    let value = "hunter2"
    let trace :: Trace Text Text
        trace = Trace{ key, deps, value }

    -- Traces are de-duplicated
    traceId1 <- insertTrace connection trace
    traceId2 <- insertTrace connection trace
    traceId1 @=? traceId2

    let expectedTraces = [trace]
    actualTraces <- fetchTraces connection Nothing
    expectedTraces @=? actualTraces
