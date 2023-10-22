module Database.DuckDB.Query
    ( module Database.DuckDB.Value

      -- * Query
    , query

      -- * Prepare statement
    , prepare
    , destroyPrepare
    , executePrepared

      -- * Prepare statement parameters
    , bindBool
    , bindInt8
    , bindInt16
    , bindInt32
    , bindInt64
    , bindUint8
    , bindUint16
    , bindUint32
    , bindUint64
    , bindFloat
    , bindDouble
    , bindDate
    , bindTime
    , bindTimestamp
    , bindVarChar
    , bindNull
    )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Int
import Data.Word
import Database.DuckDB.Internal
import Database.DuckDB.Internal.FFI
    ( DuckDBConnection
    , DuckDBPreparedStatement
    )
import Database.DuckDB.Internal.FFI qualified as FFI
import Database.DuckDB.Value
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

query :: DuckDBConnection -> String -> DuckDBMonad DuckDBResult
query conn q = do
    result <- liftIOEither $ withCString q $ \q' -> runDuckDB $ do
        result <-
            liftIO $
                FFI.duckdb_malloc (fromIntegral $ sizeOf (undefined :: FFI.DuckDBResult))
        when (result == nullPtr) $
            throwError "duckdb_malloc failed"
        err <- liftIO $ FFI.duckdb_query conn q' result
        when (isDuckDBError err) $ do
            message <- liftIO $ do
                p <- FFI.duckdb_result_error result
                peekCString p
            liftIO $ do
                FFI.duckdb_destroy_result result
                FFI.duckdb_free result
            throwError $ "duckdb_query failed: " ++ message
        return result
    return result

prepare :: DuckDBConnection -> String -> DuckDBMonad DuckDBPreparedStatement
prepare conn q = do
    stmt <- liftIOEither $ withCString q $ \q' -> alloca $ \stmt' -> runDuckDB $ do
        err <- liftIO $ FFI.duckdb_prepare conn q' stmt'
        when (isDuckDBError err) $ do
            message <- liftIO $ do
                stmt <- peek stmt'
                p <- FFI.duckdb_prepare_error stmt
                peekCString p
            liftIO $ FFI.duckdb_destroy_prepare stmt'
            throwError $ "duckdb_prepare failed: " ++ message
        liftIO $ peek stmt'
    return stmt

destroyPrepare :: DuckDBPreparedStatement -> DuckDBMonad ()
destroyPrepare stmt = do
    liftIO $ alloca $ \stmt' -> do
        poke stmt' stmt
        FFI.duckdb_destroy_prepare stmt'

prepareParameters :: DuckDBPreparedStatement -> DuckDBMonad Int
prepareParameters stmt = do
    count <- liftIO $ FFI.duckdb_nparams stmt
    return (fromIntegral count)

prepareParamType :: DuckDBPreparedStatement -> Int -> DuckDBMonad DuckDBType
prepareParamType stmt idx = liftIO $ FFI.duckdb_param_type stmt (fromIntegral idx)

prepareClearBindings :: DuckDBPreparedStatement -> DuckDBMonad ()
prepareClearBindings stmt = do
    err <- liftIO $ FFI.duckdb_clear_bindings stmt
    when (isDuckDBError err) $
        throwError "duckdb_clear_bindings failed"

executePrepared :: DuckDBPreparedStatement -> DuckDBMonad DuckDBResult
executePrepared stmt = do
    result <-
        liftIO $
            FFI.duckdb_malloc (fromIntegral $ sizeOf (undefined :: FFI.DuckDBResult))
    when (result == nullPtr) $
        throwError "duckdb_malloc failed"
    err <- liftIO $ FFI.duckdb_execute_prepared stmt result
    when (isDuckDBError err) $ do
        message <- liftIO $ do
            p <- FFI.duckdb_result_error result
            peekCString p
        liftIO $ do
            FFI.duckdb_destroy_result result
            FFI.duckdb_free result
        throwError $ "duckdb_execute_prepared failed: " ++ message
    return result

bindBool
    :: DuckDBPreparedStatement -> Int -> Bool -> DuckDBMonad ()
bindBool stmt idx val = do
    err <- liftIO $ FFI.duckdb_bind_boolean stmt (fromIntegral idx) val
    when (isDuckDBError err) $
        throwError "duckdb_bind_boolean failed"

bindInt8 :: DuckDBPreparedStatement -> Int -> Int8 -> DuckDBMonad ()
bindInt8 stmt idx val = do
    err <- liftIO $ FFI.duckdb_bind_int8 stmt (fromIntegral idx) val
    when (isDuckDBError err) $
        throwError "duckdb_bind_int8 failed"

bindInt16 :: DuckDBPreparedStatement -> Int -> Int16 -> DuckDBMonad ()
bindInt16 stmt idx val = do
    err <- liftIO $ FFI.duckdb_bind_int16 stmt (fromIntegral idx) val
    when (isDuckDBError err) $
        throwError "duckdb_bind_int16 failed"

bindInt32 :: DuckDBPreparedStatement -> Int -> Int32 -> DuckDBMonad ()
bindInt32 stmt idx val = do
    err <- liftIO $ FFI.duckdb_bind_int32 stmt (fromIntegral idx) val
    when (isDuckDBError err) $
        throwError "duckdb_bind_int32 failed"

bindInt64 :: DuckDBPreparedStatement -> Int -> Int64 -> DuckDBMonad ()
bindInt64 stmt idx val = do
    err <- liftIO $ FFI.duckdb_bind_int64 stmt (fromIntegral idx) val
    when (isDuckDBError err) $
        throwError "duckdb_bind_int64 failed"

bindUint8 :: DuckDBPreparedStatement -> Int -> Word8 -> DuckDBMonad ()
bindUint8 stmt idx val = do
    err <- liftIO $ FFI.duckdb_bind_uint8 stmt (fromIntegral idx) val
    when (isDuckDBError err) $
        throwError "duckdb_bind_uint8 failed"

bindUint16 :: DuckDBPreparedStatement -> Int -> Word16 -> DuckDBMonad ()
bindUint16 stmt idx val = do
    err <- liftIO $ FFI.duckdb_bind_uint16 stmt (fromIntegral idx) val
    when (isDuckDBError err) $
        throwError "duckdb_bind_uint16 failed"

bindUint32 :: DuckDBPreparedStatement -> Int -> Word32 -> DuckDBMonad ()
bindUint32 stmt idx val = do
    err <- liftIO $ FFI.duckdb_bind_uint32 stmt (fromIntegral idx) val
    when (isDuckDBError err) $
        throwError "duckdb_bind_uint32 failed"

bindUint64 :: DuckDBPreparedStatement -> Int -> Word64 -> DuckDBMonad ()
bindUint64 stmt idx val = do
    err <- liftIO $ FFI.duckdb_bind_uint64 stmt (fromIntegral idx) val
    when (isDuckDBError err) $
        throwError "duckdb_bind_uint64 failed"

bindFloat :: DuckDBPreparedStatement -> Int -> Float -> DuckDBMonad ()
bindFloat stmt idx val = do
    err <- liftIO $ FFI.duckdb_bind_float stmt (fromIntegral idx) val
    when (isDuckDBError err) $
        throwError "duckdb_bind_float failed"

bindDouble :: DuckDBPreparedStatement -> Int -> Double -> DuckDBMonad ()
bindDouble stmt idx val = do
    err <- liftIO $ FFI.duckdb_bind_double stmt (fromIntegral idx) val
    when (isDuckDBError err) $
        throwError "duckdb_bind_double failed"

bindDate :: DuckDBPreparedStatement -> Int -> DuckDBDate -> DuckDBMonad ()
bindDate stmt idx val = do
    err <- liftIO $ FFI.duckdb_bind_date stmt (fromIntegral idx) val
    when (isDuckDBError err) $
        throwError "duckdb_bind_date failed"

bindTime :: DuckDBPreparedStatement -> Int -> DuckDBTime -> DuckDBMonad ()
bindTime stmt idx val = do
    err <- liftIO $ FFI.duckdb_bind_time stmt (fromIntegral idx) val
    when (isDuckDBError err) $
        throwError "duckdb_bind_time failed"

bindTimestamp
    :: DuckDBPreparedStatement -> Int -> DuckDBTimestamp -> DuckDBMonad ()
bindTimestamp stmt idx val = do
    err <- liftIO $ FFI.duckdb_bind_timestamp stmt (fromIntegral idx) val
    when (isDuckDBError err) $
        throwError "duckdb_bind_timestamp failed"

bindVarChar :: DuckDBPreparedStatement -> Int -> String -> DuckDBMonad ()
bindVarChar stmt idx val = do
    liftIOEither $ withCString val $ \val' -> runDuckDB $ do
        err <- liftIO $ FFI.duckdb_bind_varchar stmt (fromIntegral idx) val'
        when (isDuckDBError err) $
            throwError "duckdb_bind_varchar failed"

bindNull :: DuckDBPreparedStatement -> Int -> DuckDBMonad ()
bindNull stmt idx = do
    err <- liftIO $ FFI.duckdb_bind_null stmt (fromIntegral idx)
    when (isDuckDBError err) $
        throwError "duckdb_bind_null failed"
