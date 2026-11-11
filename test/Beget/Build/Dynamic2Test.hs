{-# LANGUAGE TemplateHaskellQuotes #-}

module Beget.Build.Dynamic2Test where

import Beget.Build.Dynamic2
import Beget.Trace (dbCreate, dbDrop)
import Database.SQLite.Simple qualified as SQLite
import Test.Tasty.HUnit

fib :: Int -> Build Integer
fib = task 'fib \n ->
  if n < 2
    then pure (toInteger n)
    else (+) <$> fib (n - 1) <*> fib (n - 2)

unit_fib :: Assertion
unit_fib =
  SQLite.withConnection ":memory:" \connection -> do
    dbDrop connection
    dbCreate connection

    let tasks = [someTask 'fib fib]

    result <- runBuild tasks connection (fib 30)
    result @?= 832_040

    -- One trace per `fib 0` through `fib 30`
    [SQLite.Only traceCount] <- SQLite.query_ connection "select count(*) from traces"
    traceCount @?= (31 :: Int)

    result' <- runBuild tasks connection (fib 30)
    result' @?= 832_040
