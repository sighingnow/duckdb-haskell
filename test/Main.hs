module Main (main) where

import Database.DuckDB.Tests.Appender
import Database.DuckDB.Tests.Connection
import Database.DuckDB.Tests.Query
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ connectionTests
        , appenderTests
        , queryTests
        ]
