module Database.DuckDB.Tests.Connection
    ( connectionTests
    )
where

import Database.DuckDB.Connection
import Test.Tasty
import Test.Tasty.HUnit

connectionTests :: TestTree
connectionTests = testGroup "ConnectionTest" [defaultConnectionTest]

defaultConnectionTest :: TestTree
defaultConnectionTest = testCase "Setup the default duckdb database" $ do
    r <- runDuckDB $ do
        (conn, db) <- defaultConnection
        close (conn, db)
    r @?= Right ()
