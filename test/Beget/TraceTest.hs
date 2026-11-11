{-# OPTIONS_GHC -Wno-orphans #-}

module Beget.TraceTest where

import Beget.Database (dbCreate, dbDrop, withDatabase)
import Beget.Hash (BegetHash (..))
import Beget.Trace
import Beget.Value (Value)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Test.Tasty.HUnit

instance Value Text

unit_trace_roundtrip :: Assertion
unit_trace_roundtrip =
  withDatabase ":memory:" \database -> do
    dbDrop database
    dbCreate database

    let key = "password"
    let deps = HashMap.singleton "answer" (BegetHash 42)
    let value = "hunter2"
    let trace :: Trace Text Text
        trace = Trace{ key, deps, value }

    -- Traces are de-duplicated
    traceId1 <- insertTrace database trace
    traceId2 <- insertTrace database trace
    traceId1 @=? traceId2

    let expectedTraces = [trace]
    actualTraces <- fetchTraces database Nothing
    expectedTraces @=? actualTraces
