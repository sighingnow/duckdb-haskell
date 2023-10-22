module Database.DuckDB.Appender
    ( module Database.DuckDB.Value

      -- * Appender
    , appender
    , flushAppender
    , closeAppender
    , destroyAppender
    , withAppender

      -- * Appending
    , appenderBeginRow
    , appenderEndRow
    , withAppenderRow
    , appendBool
    , appendBoolUnsafe
    , appendInt8
    , appendInt8Unsafe
    , appendInt16
    , appendInt16Unsafe
    , appendInt32
    , appendInt32Unsafe
    , appendInt64
    , appendInt64Unsafe
    , appendWord8
    , appendWord8Unsafe
    , appendWord16
    , appendWord16Unsafe
    , appendWord32
    , appendWord32Unsafe
    , appendWord64
    , appendWord64Unsafe
    , appendFloat
    , appendFloatUnsafe
    , appendDouble
    , appendDoubleUnsafe
    , appendDate
    , appendDateUnsafe
    , appendTime
    , appendTimeUnsafe
    , appendTimestamp
    , appendTimestampUnsafe
    , appendString
    , appendStringUnsafe
    , appendNull
    , appendNullUnsafe
    , appendDataChunk
    )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Int
import Data.Word
import Database.DuckDB.Internal
    ( DuckDBMonad
    , isDuckDBError
    , liftIOEither
    , runDuckDB
    )
import Database.DuckDB.Internal.FFI
    ( DuckDBAppender
    , DuckDBConnection
    )
import Database.DuckDB.Internal.FFI qualified as FFI
import Database.DuckDB.Value
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

appender
    :: DuckDBConnection
    -> String
    -- ^ schema, null (empty) for default schema
    -> String
    -- ^ table name
    -> DuckDBMonad DuckDBAppender
appender conn schema table = do
    app <- liftIOEither $ withCString schema $ \schema' -> withCString table $ \table' -> do
        alloca $ \appender' -> runDuckDB $ do
            let schema'' =
                    if null schema
                        then nullPtr
                        else schema'
            err <- liftIO $ FFI.duckdb_appender_create conn schema'' table' appender'
            when (isDuckDBError err) $ do
                message <- liftIO $ do
                    app <- peek appender'
                    p <- FFI.duckdb_appender_error app
                    peekCString p
                _ <- liftIO $ FFI.duckdb_appender_destroy appender'
                throwError $ "duckdb_appender_create failed: " ++ message
            liftIO $ peek appender'
    return app

flushAppender :: DuckDBAppender -> DuckDBMonad ()
flushAppender app = do
    err <- liftIO $ FFI.duckdb_appender_flush app
    when (isDuckDBError err) $ do
        message <- liftIO $ do
            p <- FFI.duckdb_appender_error app
            peekCString p
        throwError $ "duckdb_appender_flush failed: " ++ message

closeAppender :: DuckDBAppender -> DuckDBMonad ()
closeAppender app = do
    err <- liftIO $ FFI.duckdb_appender_close app
    when (isDuckDBError err) $ do
        message <- liftIO $ do
            p <- FFI.duckdb_appender_error app
            peekCString p
        throwError $ "duckdb_appender_close failed: " ++ message

destroyAppender :: DuckDBAppender -> DuckDBMonad ()
destroyAppender app = do
    err <- liftIO $ alloca $ \appender' -> do
        poke appender' app
        FFI.duckdb_appender_destroy appender'
    when (isDuckDBError err) $ do
        message <- liftIO $ do
            p <- FFI.duckdb_appender_error app
            peekCString p
        throwError $ "duckdb_appender_destroy failed: " ++ message

withAppender
    :: DuckDBConnection
    -> String
    -- ^ schema, null (empty) for default schema
    -> String
    -- ^ table name
    -> (DuckDBAppender -> DuckDBMonad a)
    -> DuckDBMonad a
withAppender conn schema table f = do
    app <- appender conn schema table
    r <- f app
    destroyAppender app
    return r

appenderBeginRow :: DuckDBAppender -> DuckDBMonad ()
appenderBeginRow app = do
    err <- liftIO $ FFI.duckdb_appender_begin_row app
    when (isDuckDBError err) $ do
        message <- liftIO $ do
            p <- FFI.duckdb_appender_error app
            peekCString p
        throwError $ "duckdb_appender_begin_row failed: " ++ message

appenderEndRow :: DuckDBAppender -> DuckDBMonad ()
appenderEndRow app = do
    err <- liftIO $ FFI.duckdb_appender_end_row app
    when (isDuckDBError err) $ do
        message <- liftIO $ do
            p <- FFI.duckdb_appender_error app
            peekCString p
        throwError $ "duckdb_appender_end_row failed: " ++ message

withAppenderRow :: DuckDBAppender -> DuckDBMonad a -> DuckDBMonad a
withAppenderRow app f = do
    appenderBeginRow app
    r <- f
    appenderEndRow app
    return r

appendBool :: DuckDBAppender -> Bool -> DuckDBMonad ()
appendBool app b = do
    err <- liftIO $ FFI.duckdb_append_bool app b
    when (isDuckDBError err) $ do
        message <- liftIO $ do
            p <- FFI.duckdb_appender_error app
            peekCString p
        throwError $ "duckdb_append_bool failed: " ++ message

appendBoolUnsafe :: DuckDBAppender -> Bool -> DuckDBMonad ()
appendBoolUnsafe app b = liftIO $ void $ FFI.duckdb_append_bool app b

appendInt8 :: DuckDBAppender -> Int8 -> DuckDBMonad ()
appendInt8 app i = do
    err <- liftIO $ FFI.duckdb_append_int8 app i
    when (isDuckDBError err) $ do
        message <- liftIO $ do
            p <- FFI.duckdb_appender_error app
            peekCString p
        throwError $ "duckdb_append_int8 failed: " ++ message

appendInt8Unsafe :: DuckDBAppender -> Int8 -> DuckDBMonad ()
appendInt8Unsafe app i = liftIO $ void $ FFI.duckdb_append_int8 app i

appendInt16 :: DuckDBAppender -> Int16 -> DuckDBMonad ()
appendInt16 app i = do
    err <- liftIO $ FFI.duckdb_append_int16 app i
    when (isDuckDBError err) $ do
        message <- liftIO $ do
            p <- FFI.duckdb_appender_error app
            peekCString p
        throwError $ "duckdb_append_int16 failed: " ++ message

appendInt16Unsafe :: DuckDBAppender -> Int16 -> DuckDBMonad ()
appendInt16Unsafe app i = liftIO $ void $ FFI.duckdb_append_int16 app i

appendInt32 :: DuckDBAppender -> Int32 -> DuckDBMonad ()
appendInt32 app i = do
    err <- liftIO $ FFI.duckdb_append_int32 app i
    when (isDuckDBError err) $ do
        message <- liftIO $ do
            p <- FFI.duckdb_appender_error app
            peekCString p
        throwError $ "duckdb_append_int32 failed: " ++ message

appendInt32Unsafe :: DuckDBAppender -> Int32 -> DuckDBMonad ()
appendInt32Unsafe app i = liftIO $ void $ FFI.duckdb_append_int32 app i

appendInt64 :: DuckDBAppender -> Int64 -> DuckDBMonad ()
appendInt64 app i = do
    err <- liftIO $ FFI.duckdb_append_int64 app i
    when (isDuckDBError err) $ do
        message <- liftIO $ do
            p <- FFI.duckdb_appender_error app
            peekCString p
        throwError $ "duckdb_append_int64 failed: " ++ message

appendInt64Unsafe :: DuckDBAppender -> Int64 -> DuckDBMonad ()
appendInt64Unsafe app i = liftIO $ void $ FFI.duckdb_append_int64 app i

appendWord8 :: DuckDBAppender -> Word8 -> DuckDBMonad ()
appendWord8 app i = do
    err <- liftIO $ FFI.duckdb_append_uint8 app i
    when (isDuckDBError err) $ do
        message <- liftIO $ do
            p <- FFI.duckdb_appender_error app
            peekCString p
        throwError $ "duckdb_append_uint8 failed: " ++ message

appendWord8Unsafe :: DuckDBAppender -> Word8 -> DuckDBMonad ()
appendWord8Unsafe app i = liftIO $ void $ FFI.duckdb_append_uint8 app i

appendWord16 :: DuckDBAppender -> Word16 -> DuckDBMonad ()
appendWord16 app i = do
    err <- liftIO $ FFI.duckdb_append_uint16 app i
    when (isDuckDBError err) $ do
        message <- liftIO $ do
            p <- FFI.duckdb_appender_error app
            peekCString p
        throwError $ "duckdb_append_uint16 failed: " ++ message

appendWord16Unsafe :: DuckDBAppender -> Word16 -> DuckDBMonad ()
appendWord16Unsafe app i = liftIO $ void $ FFI.duckdb_append_uint16 app i

appendWord32 :: DuckDBAppender -> Word32 -> DuckDBMonad ()
appendWord32 app i = do
    err <- liftIO $ FFI.duckdb_append_uint32 app i
    when (isDuckDBError err) $ do
        message <- liftIO $ do
            p <- FFI.duckdb_appender_error app
            peekCString p
        throwError $ "duckdb_append_uint32 failed: " ++ message

appendWord32Unsafe :: DuckDBAppender -> Word32 -> DuckDBMonad ()
appendWord32Unsafe app i = liftIO $ void $ FFI.duckdb_append_uint32 app i

appendWord64 :: DuckDBAppender -> Word64 -> DuckDBMonad ()
appendWord64 app i = do
    err <- liftIO $ FFI.duckdb_append_uint64 app i
    when (isDuckDBError err) $ do
        message <- liftIO $ do
            p <- FFI.duckdb_appender_error app
            peekCString p
        throwError $ "duckdb_append_uint64 failed: " ++ message

appendWord64Unsafe :: DuckDBAppender -> Word64 -> DuckDBMonad ()
appendWord64Unsafe app i = liftIO $ void $ FFI.duckdb_append_uint64 app i

appendFloat :: DuckDBAppender -> Float -> DuckDBMonad ()
appendFloat app f = do
    err <- liftIO $ FFI.duckdb_append_float app f
    when (isDuckDBError err) $ do
        message <- liftIO $ do
            p <- FFI.duckdb_appender_error app
            peekCString p
        throwError $ "duckdb_append_float failed: " ++ message

appendFloatUnsafe :: DuckDBAppender -> Float -> DuckDBMonad ()
appendFloatUnsafe app f = liftIO $ void $ FFI.duckdb_append_float app f

appendDouble :: DuckDBAppender -> Double -> DuckDBMonad ()
appendDouble app d = do
    err <- liftIO $ FFI.duckdb_append_double app d
    when (isDuckDBError err) $ do
        message <- liftIO $ do
            p <- FFI.duckdb_appender_error app
            peekCString p
        throwError $ "duckdb_append_double failed: " ++ message

appendDoubleUnsafe :: DuckDBAppender -> Double -> DuckDBMonad ()
appendDoubleUnsafe app d = liftIO $ void $ FFI.duckdb_append_double app d

appendDate :: DuckDBAppender -> Int32 -> DuckDBMonad ()
appendDate app d = do
    err <- liftIO $ FFI.duckdb_append_date app d
    when (isDuckDBError err) $ do
        message <- liftIO $ do
            p <- FFI.duckdb_appender_error app
            peekCString p
        throwError $ "duckdb_append_date failed: " ++ message

appendDateUnsafe :: DuckDBAppender -> Int32 -> DuckDBMonad ()
appendDateUnsafe app d = liftIO $ void $ FFI.duckdb_append_date app d

appendTime :: DuckDBAppender -> Int64 -> DuckDBMonad ()
appendTime app t = do
    err <- liftIO $ FFI.duckdb_append_time app t
    when (isDuckDBError err) $ do
        message <- liftIO $ do
            p <- FFI.duckdb_appender_error app
            peekCString p
        throwError $ "duckdb_append_time failed: " ++ message

appendTimeUnsafe :: DuckDBAppender -> Int64 -> DuckDBMonad ()
appendTimeUnsafe app t = liftIO $ void $ FFI.duckdb_append_time app t

appendTimestamp :: DuckDBAppender -> Int64 -> DuckDBMonad ()
appendTimestamp app t = do
    err <- liftIO $ FFI.duckdb_append_timestamp app t
    when (isDuckDBError err) $ do
        message <- liftIO $ do
            p <- FFI.duckdb_appender_error app
            peekCString p
        throwError $ "duckdb_append_timestamp failed: " ++ message

appendTimestampUnsafe :: DuckDBAppender -> Int64 -> DuckDBMonad ()
appendTimestampUnsafe app t = liftIO $ void $ FFI.duckdb_append_timestamp app t

appendString :: DuckDBAppender -> String -> DuckDBMonad ()
appendString app s = do
    err <- liftIO $ withCString s $ \s' -> FFI.duckdb_append_varchar app s'
    when (isDuckDBError err) $ do
        message <- liftIO $ do
            p <- FFI.duckdb_appender_error app
            peekCString p
        throwError $ "duckdb_append_varchar failed: " ++ message

appendStringUnsafe :: DuckDBAppender -> String -> DuckDBMonad ()
appendStringUnsafe app s = liftIO $ void $ withCString s $ \s' -> FFI.duckdb_append_varchar app s'

appendNull :: DuckDBAppender -> DuckDBMonad ()
appendNull app = do
    err <- liftIO $ FFI.duckdb_append_null app
    when (isDuckDBError err) $ do
        message <- liftIO $ do
            p <- FFI.duckdb_appender_error app
            peekCString p
        throwError $ "duckdb_append_null failed: " ++ message

appendNullUnsafe :: DuckDBAppender -> DuckDBMonad ()
appendNullUnsafe app = liftIO $ void $ FFI.duckdb_append_null app

appendDataChunk :: DuckDBAppender -> DuckDBDataChunk -> DuckDBMonad ()
appendDataChunk app chk = do
    err <- liftIO $ FFI.duckdb_append_data_chunk app chk
    when (isDuckDBError err) $ do
        message <- liftIO $ do
            p <- FFI.duckdb_appender_error app
            peekCString p
        throwError $ "duckdb_append_chunk failed: " ++ message
