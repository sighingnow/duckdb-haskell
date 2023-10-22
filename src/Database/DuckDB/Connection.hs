module Database.DuckDB.Connection
    ( -- * Error reporting
      DuckDBError
    , isDuckDBError

      -- * Monad
    , DuckDBMonad (..)
    , liftIO
    , liftIOEither
    , runDuckDB

      -- * Version
    , version

      -- * Database and Connection
    , open
    , configure
    , configCount
    , getConfigFlag
    , setConfig
    , destroyConfig
    , openExt
    , connect
    , defaultConnection
    , withDefaultConnection
    , close
    , closeConnection
    , closeDatabase
    )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Database.DuckDB.Internal
import Database.DuckDB.Internal.FFI
    ( DuckDBConfig
    , DuckDBConnection
    , DuckDBDatabase
    )
import Database.DuckDB.Internal.FFI qualified as FFI
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable

open :: String -> DuckDBMonad DuckDBDatabase
open path = do
    db <- liftIOEither $ withCString path $ \path' -> runDuckDB $ do
        liftIOEither $ alloca $ \db' -> runDuckDB $ do
            err <- liftIO $ FFI.duckdb_open path' db'
            when (isDuckDBError err) $
                throwError "duckdb_open failed"
            liftIO $ peek db'
    return db

configure :: DuckDBMonad DuckDBConfig
configure = do
    config <- liftIOEither $ alloca $ \config' -> runDuckDB $ do
        err <- liftIO $ FFI.duckdb_create_config config'
        when (isDuckDBError err) $
            throwError "duckdb_create_config failed"
        liftIO $ peek config'
    return config

configCount :: DuckDBMonad Int
configCount = do
    count <- liftIO $ FFI.duckdb_config_count
    return (fromIntegral count)

getConfigFlag
    :: Int
    -> DuckDBMonad
        ( String
        , -- \^ name
          String
        )
-- \^ description

getConfigFlag index = do
    (name, description) <- liftIOEither $ alloca $ \name' -> alloca $ \description' -> runDuckDB $ do
        err <-
            liftIO $ FFI.duckdb_get_config_flag (fromIntegral index) name' description'
        when (isDuckDBError err) $
            throwError "duckdb_config_get_flag failed"
        liftIO $
            (,) <$> (peek name' >>= peekCString) <*> (peek description' >>= peekCString)
    return (name, description)

setConfig
    :: DuckDBConfig
    -> String
    -- ^ name
    -> String
    -- ^ option
    -> DuckDBMonad ()
setConfig config name option = do
    liftIOEither $ withCString name $ \name' -> withCString option $ \option' -> runDuckDB $ do
        err <- liftIO $ FFI.duckdb_set_config config name' option'
        when (isDuckDBError err) $
            throwError "duckdb_set_config failed"

destroyConfig :: DuckDBConfig -> DuckDBMonad ()
destroyConfig config = do
    liftIO $ alloca $ \config' -> do
        poke config' config
        FFI.duckdb_destroy_config config'

openExt :: String -> DuckDBConfig -> DuckDBMonad DuckDBDatabase
openExt path config = do
    db <- liftIOEither $ withCString path $ \path' -> runDuckDB $ do
        liftIOEither $ alloca $ \db' ->
            alloca $ \err' -> runDuckDB $ do
                err <- liftIO $ FFI.duckdb_open_ext path' db' config err'
                when (isDuckDBError err) $ do
                    message <- liftIO $ do
                        p <- peek err'
                        m <- peekCString p
                        FFI.duckdb_free p
                        return m
                    throwError $ "duckdb_open failed: " ++ message
                liftIO $ peek db'
    return db

connect :: DuckDBDatabase -> DuckDBMonad DuckDBConnection
connect db = do
    conn <- liftIOEither $ alloca $ \conn' -> runDuckDB $ do
        err <- liftIO $ FFI.duckdb_connect db conn'
        when (isDuckDBError err) $
            throwError "duckdb_connect failed"
        liftIO $ peek conn'
    return conn

defaultConnection :: DuckDBMonad (DuckDBDatabase, DuckDBConnection)
defaultConnection = do
    db <- open ":memory:"
    conn <- connect db
    return (db, conn)

withDefaultConnection
    :: ((DuckDBDatabase, DuckDBConnection) -> DuckDBMonad a) -> DuckDBMonad a
withDefaultConnection f = do
    (db, conn) <- defaultConnection
    r <- f (db, conn)
    close (db, conn)
    return r

close :: (DuckDBDatabase, DuckDBConnection) -> DuckDBMonad ()
close (db, conn) = do
    closeConnection conn
    closeDatabase db

closeConnection :: DuckDBConnection -> DuckDBMonad ()
closeConnection conn = do
    liftIO $ alloca $ \conn' -> do
        poke conn' conn
        FFI.duckdb_disconnect conn'

closeDatabase :: DuckDBDatabase -> DuckDBMonad ()
closeDatabase db = do
    liftIO $ alloca $ \db' -> do
        poke db' db
        FFI.duckdb_close db'
