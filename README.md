# duckdb-haskell

Full-featured Haskell bindings for [DuckDB](https://duckdb.org).

## Installation

The library is available on [Hackage](https://hackage.haskell.org/package/duckdb-haskell).

## Usage

- `Database.DuckDB.Internal.FFI`: fully FFI bindings to [the DuckDB C API](https://github.com/duckdb/duckdb/blob/main/src/include/duckdb.h)
- `Database.DuckDB.Connection`: connection management
- `Database.DuckDB.Query`: query execution
- `Database.DuckDB.Appender`: bulk data loading

### Connection

- Connect to a database:

    ```haskell
    defaultConnectionTest :: TestTree
    defaultConnectionTest = testCase "Setup the default duckdb database" $ do
        r <- runDuckDB $ do
            (conn, db) <- defaultConnection
            close (conn, db)
        r @?= Right ()
    ```

### Query results

- Query from the database:

    ```haskell
    query42Test :: TestTree
    query42Test = testCase "Query the constant (42)" $ do
        r <- runDuckDB $ withDefaultConnection $ \(_db, conn) -> do
            r <- query conn "select 42;"
            v <- valueInt32 r 0 0
            liftIO $ v @?= 42
        r @?= Right ()
    ```

- Query from the database, more complex example:

    ```haskell
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
    ```

- Query from the database, inspecting the value using [`Data.Vector.Storable`](https://hackage.haskell.org/package/vector):

    ```
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
    ```

### Bulk data loading

- Efficiently loading data using appenders:

    ```haskell
    appenderTableTest :: TestTree
    appenderTableTest = testCase "Create table, append data and query" $ do
        r <- runDuckDB $ withDefaultConnection $ \(_db, conn) -> do
            _ <- query conn "CREATE TABLE integers (i INTEGER, j INTEGER)"

            withAppender conn "" "integers" $ \app ->
                forM_ [1 .. 100] $ \i -> withAppenderRow app $ do
                    appendInt32 app i
                    appendInt32 app (i + 99)

            r <- query conn "SELECT i, j FROM integers"

            liftIO $ chunkCount r >>= print
    ```
