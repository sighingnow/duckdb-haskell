module Database.DuckDB.Tests.Appender
    ( appenderTests
    )
where

import Control.Monad
import Data.Vector.Storable qualified as Vec
import Database.DuckDB.Appender
import Database.DuckDB.Connection
import Database.DuckDB.Query
import Foreign.ForeignPtr
import Test.Tasty
import Test.Tasty.HUnit

appenderTests :: TestTree
appenderTests =
    testGroup
        "AppendTableTest"
        [ appenderTableTest
        ]

appenderTableTest :: TestTree
appenderTableTest = testCase "Create table, append data and query" $ do
    r <- runDuckDB $ withDefaultConnection $ \(_db, conn) -> do
        _ <- query conn "CREATE TABLE integers (i INTEGER, j INTEGER)"

        withAppender conn "" "integers" $ \app ->
            forM_ [1 .. 100] $ \i -> withAppenderRow app $ do
                appendInt32 app i
                appendInt32 app (i + 99)

        r <- query conn "SELECT i, j FROM integers"

        nchk <- chunkCount r
        liftIO $ nchk @?= 1

        chk <- chunkAt r 0
        columns <- getChunkColumnCount chk
        liftIO $ columns @?= 2

        rows <- getChunkSize chk
        liftIO $ rows @?= 100

        getChunkVector chk 0 >>= getVectorData >>= \pointer ->
            liftIO $ do
                vec <-
                    Vec.unsafeFromForeignPtr0 <$> newForeignPtr_ pointer <*> (pure rows)
                        :: IO (Vec.Vector Int32)
                vec @?= Vec.fromList [1 .. 100]

        getChunkVector chk 1 >>= getVectorData >>= \pointer ->
            liftIO $ do
                vec <-
                    Vec.unsafeFromForeignPtr0 <$> newForeignPtr_ pointer <*> (pure rows)
                        :: IO (Vec.Vector Int32)
                vec @?= Vec.fromList [100 .. 199]

    r @?= Right ()
