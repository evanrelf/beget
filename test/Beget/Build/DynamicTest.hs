{-# LANGUAGE TemplateHaskell #-}

module Beget.Build.DynamicTest where

import Beget.Build.Dynamic
import Beget.Trace (dbCreate, dbDrop, withConnection, withDatabase)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Database.SQLite.Simple qualified as SQLite
import Prelude hiding (concat, readFile)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty.HUnit

fib :: Int -> Build Integer
fib = task 'fib \n ->
  if n < 2
    then pure (toInteger n)
    else (+) <$> fib (n - 1) <*> fib (n - 2)

registerTask 'fib

unit_fib :: Assertion
unit_fib =
  withDatabase ":memory:" \database -> do
    dbDrop database
    dbCreate database

    let tasks = $$discoverTasks

    result <- runBuild tasks database (fib 30)
    result @?= 832_040

    -- One trace per `fib 0` through `fib 30`
    [SQLite.Only traceCount] <- withConnection database \connection ->
      SQLite.query_ connection "select count(*) from traces"
    traceCount @?= (31 :: Int)

    result' <- runBuild tasks database (fib 30)
    result' @?= 832_040

readFile :: FilePath -> Build ByteString
readFile = task 'readFile \path ->
  liftIO $ ByteString.readFile path

registerTask 'readFile

concat :: FilePath -> Build ByteString
concat = task 'concat \path -> do
  bytes <- readFile path
  let text = Text.decodeUtf8 bytes
  let paths = map Text.unpack (Text.lines text)
  mconcat <$> traverse readFile paths

registerTask 'concat

unit_volatile :: Assertion
unit_volatile =
  withSystemTempDirectory "beget" \dir ->
  withDatabase ":memory:" \database -> do
    dbDrop database
    dbCreate database

    let tasks = $$discoverTasks

    let write name contents = do
          ByteString.writeFile (dir </> name) (Text.encodeUtf8 contents)
    write "files" (Text.unlines (map Text.pack [dir </> "a", dir </> "a", dir </> "b"]))
    write "a" "AAAA\n"
    write "b" "BBBB\n"

    result <- runBuild tasks database do
      concat (dir </> "files")
    result @?= "AAAA\nAAAA\nBBBB\n"

    -- `readFile` does IO, so only `concat` is cached
    [SQLite.Only traceCount] <- withConnection database \connection ->
      SQLite.query_ connection "select count(*) from traces"
    traceCount @?= (1 :: Int)

    write "b" "bbbb\n"

    result' <- runBuild tasks database do
      concat (dir </> "files")
    result' @?= "AAAA\nAAAA\nbbbb\n"
