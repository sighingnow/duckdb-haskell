module Database.DuckDB.Tests.Query
    ( queryTests
    )
where

import Data.Vector.Storable qualified as Vec
import Database.DuckDB.Connection
import Database.DuckDB.Query
import Foreign.ForeignPtr
import Test.Tasty
import Test.Tasty.HUnit

queryTests :: TestTree
queryTests =
    testGroup
        "QueryTest"
        [ query42Test
        , queryCreateTableTest
        , queryDataVector
        ]

query42Test :: TestTree
query42Test = testCase "Query the constant (42)" $ do
    r <- runDuckDB $ withDefaultConnection $ \(_db, conn) -> do
        r <- query conn "select 42;"
        v <- valueInt32 r 0 0
        liftIO $ v @?= 42
    r @?= Right ()

queryCreateTableTest :: TestTree
queryCreateTableTest = testCase "Create table and query" $ do
    r <- runDuckDB $ withDefaultConnection $ \(_db, conn) -> do
        _ <- query conn "CREATE TABLE integers (i INTEGER)"
        _ <- query conn "INSERT INTO integers VALUES (1), (2), (3), (999)"
        r <- query conn "SELECT i FROM integers"
        valueInt32 r 0 0 >>= \v -> liftIO $ v @?= 1
        valueInt32 r 0 1 >>= \v -> liftIO $ v @?= 2
        valueInt32 r 0 2 >>= \v -> liftIO $ v @?= 3
        valueInt32 r 0 3 >>= \v -> liftIO $ v @?= 999
    r @?= Right ()

queryDataVector :: TestTree
queryDataVector = testCase "Create table and query as efficient vectors" $ do
    r <- runDuckDB $ withDefaultConnection $ \(_db, conn) -> do
        _ <- query conn "CREATE TABLE integers (i INTEGER)"
        _ <- query conn "INSERT INTO integers VALUES (1), (2), (3), (999)"
        r <- query conn "SELECT i FROM integers"

        nchk <- chunkCount r
        liftIO $ nchk @?= 1

        chk <- chunkAt r 0
        columns <- getChunkColumnCount chk
        liftIO $ columns @?= 1

        rows <- getChunkSize chk
        liftIO $ rows @?= 4

        pointer <- getVectorData =<< getChunkVector chk 0

        liftIO $ do
            vec <-
                Vec.unsafeFromForeignPtr0 <$> newForeignPtr_ pointer <*> (pure rows)
                    :: IO (Vec.Vector Int32)
            vec @?= Vec.fromList [1, 2, 3, 999]
    r @?= Right ()
