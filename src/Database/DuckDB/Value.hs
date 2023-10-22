{-# LANGUAGE PatternSynonyms #-}

module Database.DuckDB.Value
    ( -- * Query result
      DuckDBResult
    , DuckDBDataChunk
    , DuckDBVector
    , DuckDBType
    , DuckDBLogicalType
    , destroy

      -- * Data types
    , pattern FFI.DUCKDB_TYPE_INVALID
    , pattern FFI.DUCKDB_TYPE_BOOLEAN
    , pattern FFI.DUCKDB_TYPE_TINYINT
    , pattern FFI.DUCKDB_TYPE_SMALLINT
    , pattern FFI.DUCKDB_TYPE_INTEGER
    , pattern FFI.DUCKDB_TYPE_BIGINT
    , pattern FFI.DUCKDB_TYPE_UTINYINT
    , pattern FFI.DUCKDB_TYPE_USMALLINT
    , pattern FFI.DUCKDB_TYPE_UINTEGER
    , pattern FFI.DUCKDB_TYPE_UBIGINT
    , pattern FFI.DUCKDB_TYPE_FLOAT
    , pattern FFI.DUCKDB_TYPE_DOUBLE
    , pattern FFI.DUCKDB_TYPE_TIMESTAMP
    , pattern FFI.DUCKDB_TYPE_DATE
    , pattern FFI.DUCKDB_TYPE_TIME
    , pattern FFI.DUCKDB_TYPE_INTERVAL
    , pattern FFI.DUCKDB_TYPE_HUGEINT
    , pattern FFI.DUCKDB_TYPE_VARCHAR
    , pattern FFI.DUCKDB_TYPE_BLOB
    , pattern FFI.DUCKDB_TYPE_DECIMAL
    , pattern FFI.DUCKDB_TYPE_TIMESTAMP_S
    , pattern FFI.DUCKDB_TYPE_TIMESTAMP_MS
    , pattern FFI.DUCKDB_TYPE_TIMESTAMP_NS
    , pattern FFI.DUCKDB_TYPE_ENUM
    , pattern FFI.DUCKDB_TYPE_LIST
    , pattern FFI.DUCKDB_TYPE_STRUCT
    , pattern FFI.DUCKDB_TYPE_MAP
    , pattern FFI.DUCKDB_TYPE_UUID
    , pattern FFI.DUCKDB_TYPE_UNION
    , pattern FFI.DUCKDB_TYPE_BIT
    , typeRep
    , Int8
    , Int16
    , Int32
    , Int64
    , Word8
    , Word16
    , Word32
    , Word64
    , DuckDBDate
    , DuckDBTime
    , DuckDBTimestamp

      -- * Columns
    , columnName
    , columnType
    , columnLogicalType
    , columnCount
    , rowCount
    , rowsChanged
    , columnData
    , nullmaskData

      -- * Data chunks
    , chunkAt
    , isStreaming
    , chunkCount
    , createChunk
    , destroyChunk
    , resetChunk
    , getChunkColumnCount
    , getChunkVector
    , getChunkSize
    , setChunkSize
    , getVectorType
    , getVectorData
    , getVectorValidity
    , ensureVectorValidityWritable
    , setVectorStringValue
    , isValidityRowValid
    , setValidityRow
    , setValidityRowInvalid
    , setValidityRowValid

      -- * Element values
    , valueBoolean
    , valueInt8
    , valueInt16
    , valueInt32
    , valueInt64
    , valueUint8
    , valueUint16
    , valueUint32
    , valueUint64
    , valueFloat
    , valueDouble
    , valueDate
    , valueTime
    , valueTimestamp
    , valueVarChar
    , valueVarCharInternal
    , valueIsNull
    )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Coerce
import Data.Int
import Data.Typeable (TypeRep, typeOf)
import Data.Word
import Database.DuckDB.Internal
import Database.DuckDB.Internal.FFI
    ( DuckDBDataChunk
    , DuckDBDate
    , DuckDBLogicalType
    , DuckDBTime
    , DuckDBTimestamp
    , DuckDBType
    , DuckDBVector
    )
import Database.DuckDB.Internal.FFI qualified as FFI
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

type DuckDBResult = Ptr FFI.DuckDBResult

destroy :: DuckDBResult -> DuckDBMonad ()
destroy result = liftIO $ do
    FFI.duckdb_destroy_result result
    FFI.duckdb_free result

typeRep :: DuckDBType -> TypeRep
typeRep FFI.DUCKDB_TYPE_INVALID = typeOf ()
typeRep FFI.DUCKDB_TYPE_BOOLEAN = typeOf (undefined :: Bool)
typeRep FFI.DUCKDB_TYPE_TINYINT = typeOf (undefined :: Int8)
typeRep FFI.DUCKDB_TYPE_SMALLINT = typeOf (undefined :: Int16)
typeRep FFI.DUCKDB_TYPE_INTEGER = typeOf (undefined :: Int32)
typeRep FFI.DUCKDB_TYPE_BIGINT = typeOf (undefined :: Int64)
typeRep FFI.DUCKDB_TYPE_UTINYINT = typeOf (undefined :: Word8)
typeRep FFI.DUCKDB_TYPE_USMALLINT = typeOf (undefined :: Word16)
typeRep FFI.DUCKDB_TYPE_UINTEGER = typeOf (undefined :: Word32)
typeRep FFI.DUCKDB_TYPE_UBIGINT = typeOf (undefined :: Word64)
typeRep FFI.DUCKDB_TYPE_FLOAT = typeOf (undefined :: Float)
typeRep FFI.DUCKDB_TYPE_DOUBLE = typeOf (undefined :: Double)
typeRep FFI.DUCKDB_TYPE_TIMESTAMP = typeOf (undefined :: Int64)
typeRep FFI.DUCKDB_TYPE_DATE = typeOf (undefined :: Int32)
typeRep FFI.DUCKDB_TYPE_TIME = typeOf (undefined :: Int64)
typeRep FFI.DUCKDB_TYPE_INTERVAL = typeOf (undefined :: Int64)
typeRep FFI.DUCKDB_TYPE_HUGEINT = typeOf (undefined :: Int64)
typeRep FFI.DUCKDB_TYPE_VARCHAR = typeOf (undefined :: String)
typeRep FFI.DUCKDB_TYPE_BLOB = typeOf (undefined :: String)
typeRep FFI.DUCKDB_TYPE_DECIMAL = typeOf (undefined :: Double)
typeRep FFI.DUCKDB_TYPE_TIMESTAMP_S = typeOf (undefined :: Int64)
typeRep FFI.DUCKDB_TYPE_TIMESTAMP_MS = typeOf (undefined :: Int64)
typeRep FFI.DUCKDB_TYPE_TIMESTAMP_NS = typeOf (undefined :: Int64)
typeRep FFI.DUCKDB_TYPE_ENUM = typeOf (undefined :: Int32)
typeRep FFI.DUCKDB_TYPE_LIST = typeOf (undefined :: Int64)
typeRep FFI.DUCKDB_TYPE_STRUCT = typeOf (undefined :: Int64)
typeRep FFI.DUCKDB_TYPE_MAP = typeOf (undefined :: Int64)
typeRep FFI.DUCKDB_TYPE_UUID = typeOf (undefined :: Int64)
typeRep FFI.DUCKDB_TYPE_UNION = typeOf (undefined :: Int64)
typeRep FFI.DUCKDB_TYPE_BIT = typeOf (undefined :: Int64)
typeRep ty = error ("impossible: unknown duckdb type enumeration: " ++ show ty)

columnName :: DuckDBResult -> Int -> DuckDBMonad String
columnName result idx = do
    name' <- liftIO $ FFI.duckdb_column_name result (fromIntegral idx)
    when (name' == nullPtr) $
        throwError "duckdb_column_name failed"
    liftIO $ peekCString name'

columnType :: DuckDBResult -> Int -> DuckDBMonad DuckDBType
columnType result idx = do
    ty <- liftIO $ FFI.duckdb_column_type result (fromIntegral idx)
    when (ty == FFI.DUCKDB_TYPE_INVALID) $
        throwError "duckdb_column_type failed"
    return ty

columnLogicalType :: DuckDBResult -> Int -> DuckDBMonad DuckDBLogicalType
columnLogicalType result idx = do
    ty <- liftIO $ FFI.duckdb_column_logical_type result (fromIntegral idx)
    when (coerce ty == nullPtr) $
        throwError "duckdb_column_logical_type failed"
    return ty

columnCount :: DuckDBResult -> DuckDBMonad Int
columnCount result = do
    count <- liftIO $ FFI.duckdb_column_count result
    return (fromIntegral count)

rowCount :: DuckDBResult -> DuckDBMonad Int
rowCount result = do
    count <- liftIO $ FFI.duckdb_row_count result
    return (fromIntegral count)

rowsChanged :: DuckDBResult -> DuckDBMonad Int
rowsChanged result = do
    count <- liftIO $ FFI.duckdb_rows_changed result
    return (fromIntegral count)

columnData :: DuckDBResult -> Int -> DuckDBMonad (Ptr a)
columnData result idx = do
    data' <- liftIO $ FFI.duckdb_column_data result (fromIntegral idx)
    when (data' == nullPtr) $
        throwError "duckdb_column_data failed"
    return data'

nullmaskData :: DuckDBResult -> Int -> DuckDBMonad (Ptr Bool)
nullmaskData result idx = do
    data' <- liftIO $ FFI.duckdb_nullmask_data result (fromIntegral idx)
    when (data' == nullPtr) $
        throwError "duckdb_nullmask_data failed"
    return data'

chunkAt :: DuckDBResult -> Int -> DuckDBMonad DuckDBDataChunk
chunkAt result idx = do
    chk <- liftIO $ do
        FFI.duckdb_result_get_chunk result (fromIntegral idx)
    return chk

isStreaming :: DuckDBResult -> DuckDBMonad Bool
isStreaming result = do
    streaming <- liftIO $ do
        FFI.duckdb_result_is_streaming result
    return streaming

chunkCount :: DuckDBResult -> DuckDBMonad Int
chunkCount result = do
    count <- liftIO $ do
        FFI.duckdb_result_chunk_count result
    return (fromIntegral count)

createChunk
    :: [DuckDBLogicalType]
    -> Int
    -- ^ column count
    -> DuckDBMonad DuckDBDataChunk
createChunk tys colCount = do
    chk <- liftIO $ withArray tys $ \tys' -> FFI.duckdb_create_data_chunk tys' (fromIntegral colCount)
    when (coerce chk == nullPtr) $
        throwError "duckdb_create_data_chunk failed"
    return chk

destroyChunk :: DuckDBDataChunk -> DuckDBMonad ()
destroyChunk chk = liftIO $ alloca $ \chk' -> do
    poke chk' chk
    FFI.duckdb_destroy_data_chunk chk'

resetChunk :: DuckDBDataChunk -> DuckDBMonad ()
resetChunk chk = liftIO $ FFI.duckdb_data_chunk_reset chk

getChunkColumnCount :: DuckDBDataChunk -> DuckDBMonad Int
getChunkColumnCount chk = do
    count <- liftIO $ FFI.duckdb_data_chunk_get_column_count chk
    return (fromIntegral count)

getChunkVector :: DuckDBDataChunk -> Int -> DuckDBMonad DuckDBVector
getChunkVector chk idx = do
    vec <- liftIO $ FFI.duckdb_data_chunk_get_vector chk (fromIntegral idx)
    when (coerce vec == nullPtr) $
        throwError "duckdb_data_chunk_get_vector failed"
    return vec

getChunkSize :: DuckDBDataChunk -> DuckDBMonad Int
getChunkSize chk = do
    size <- liftIO $ FFI.duckdb_data_chunk_get_size chk
    return (fromIntegral size)

setChunkSize :: DuckDBDataChunk -> Int -> DuckDBMonad ()
setChunkSize chk size = liftIO $ FFI.duckdb_data_chunk_set_size chk (fromIntegral size)

getVectorType :: DuckDBVector -> DuckDBMonad DuckDBLogicalType
getVectorType vec = liftIO $ FFI.duckdb_vector_get_column_type vec

getVectorData :: DuckDBVector -> DuckDBMonad (Ptr a)
getVectorData vec = liftIO $ FFI.duckdb_vector_get_data vec

getVectorValidity :: DuckDBVector -> DuckDBMonad (Ptr Word64)
getVectorValidity vec = liftIO $ FFI.duckdb_vector_get_validity vec

ensureVectorValidityWritable :: DuckDBVector -> DuckDBMonad ()
ensureVectorValidityWritable vec = liftIO $ FFI.duckdb_vector_ensure_validity_writable vec

setVectorStringValue :: DuckDBVector -> Int -> String -> DuckDBMonad ()
setVectorStringValue vec idx value = liftIO $ withCString value $ \value' ->
    FFI.duckdb_vector_assign_string_element vec (fromIntegral idx) value'

isValidityRowValid :: Ptr Word64 -> Int -> DuckDBMonad Bool
isValidityRowValid vec idx = liftIO $ FFI.duckdb_validity_row_is_valid vec (fromIntegral idx)

setValidityRow :: Ptr Word64 -> Int -> Bool -> DuckDBMonad ()
setValidityRow vec idx valid = liftIO $ FFI.duckdb_validity_set_row_validity vec (fromIntegral idx) valid

setValidityRowInvalid :: Ptr Word64 -> Int -> DuckDBMonad ()
setValidityRowInvalid vec idx = liftIO $ FFI.duckdb_validity_set_row_invalid vec (fromIntegral idx)

setValidityRowValid :: Ptr Word64 -> Int -> DuckDBMonad ()
setValidityRowValid vec idx = liftIO $ FFI.duckdb_validity_set_row_valid vec (fromIntegral idx)

valueBoolean
    :: DuckDBResult
    -> Int
    -- ^ column
    -> Int
    -- ^ row
    -> DuckDBMonad Bool
valueBoolean result col row =
    liftIO $ FFI.duckdb_value_boolean result (fromIntegral col) (fromIntegral row)

valueInt8
    :: DuckDBResult
    -> Int
    -- ^ column
    -> Int
    -- ^ row
    -> DuckDBMonad Int8
valueInt8 result col row = liftIO $ FFI.duckdb_value_int8 result (fromIntegral col) (fromIntegral row)

valueInt16
    :: DuckDBResult
    -> Int
    -- ^ column
    -> Int
    -- ^ row
    -> DuckDBMonad Int16
valueInt16 result col row = liftIO $ FFI.duckdb_value_int16 result (fromIntegral col) (fromIntegral row)

valueInt32
    :: DuckDBResult
    -> Int
    -- ^ column
    -> Int
    -- ^ row
    -> DuckDBMonad Int32
valueInt32 result col row = liftIO $ FFI.duckdb_value_int32 result (fromIntegral col) (fromIntegral row)

valueInt64
    :: DuckDBResult
    -> Int
    -- ^ column
    -> Int
    -- ^ row
    -> DuckDBMonad Int64
valueInt64 result col row = liftIO $ FFI.duckdb_value_int64 result (fromIntegral col) (fromIntegral row)

valueUint8
    :: DuckDBResult
    -> Int
    -- ^ column
    -> Int
    -- ^ row
    -> DuckDBMonad Word8
valueUint8 result col row = liftIO $ FFI.duckdb_value_uint8 result (fromIntegral col) (fromIntegral row)

valueUint16
    :: DuckDBResult
    -> Int
    -- ^ column
    -> Int
    -- ^ row
    -> DuckDBMonad Word16
valueUint16 result col row = liftIO $ FFI.duckdb_value_uint16 result (fromIntegral col) (fromIntegral row)

valueUint32
    :: DuckDBResult
    -> Int
    -- ^ column
    -> Int
    -- ^ row
    -> DuckDBMonad Word32
valueUint32 result col row = liftIO $ FFI.duckdb_value_uint32 result (fromIntegral col) (fromIntegral row)

valueUint64
    :: DuckDBResult
    -> Int
    -- ^ column
    -> Int
    -- ^ row
    -> DuckDBMonad Word64
valueUint64 result col row = liftIO $ FFI.duckdb_value_uint64 result (fromIntegral col) (fromIntegral row)

valueFloat
    :: DuckDBResult
    -> Int
    -- ^ column
    -> Int
    -- ^ row
    -> DuckDBMonad Float
valueFloat result col row = liftIO $ FFI.duckdb_value_float result (fromIntegral col) (fromIntegral row)

valueDouble
    :: DuckDBResult
    -> Int
    -- ^ column
    -> Int
    -- ^ row
    -> DuckDBMonad Double
valueDouble result col row = liftIO $ FFI.duckdb_value_double result (fromIntegral col) (fromIntegral row)

valueDate
    :: DuckDBResult
    -> Int
    -- ^ column
    -> Int
    -- ^ row
    -> DuckDBMonad DuckDBDate
valueDate result col row = liftIO $ FFI.duckdb_value_date result (fromIntegral col) (fromIntegral row)

valueTime
    :: DuckDBResult
    -> Int
    -- ^ column
    -> Int
    -- ^ row
    -> DuckDBMonad DuckDBTime
valueTime result col row = liftIO $ FFI.duckdb_value_time result (fromIntegral col) (fromIntegral row)

valueTimestamp
    :: DuckDBResult
    -> Int
    -- ^ column
    -> Int
    -- ^ row
    -> DuckDBMonad DuckDBTimestamp
valueTimestamp result col row =
    liftIO $ FFI.duckdb_value_timestamp result (fromIntegral col) (fromIntegral row)

valueVarChar
    :: DuckDBResult
    -> Int
    -- ^ column
    -> Int
    -- ^ row
    -> DuckDBMonad String
valueVarChar result col row = do
    value <-
        liftIO $ FFI.duckdb_value_varchar result (fromIntegral col) (fromIntegral row)
    when (value == nullPtr) $
        throwError "duckdb_value_varchar failed"
    liftIO $ peekCString value

valueVarCharInternal
    :: DuckDBResult
    -> Int
    -- ^ column
    -> Int
    -- ^ row
    -> DuckDBMonad CString
valueVarCharInternal result col row =
    liftIO $
        FFI.duckdb_value_varchar_internal result (fromIntegral col) (fromIntegral row)

valueIsNull
    :: DuckDBResult
    -> Int
    -- ^ column
    -> Int
    -- ^ row
    -> DuckDBMonad Bool
valueIsNull result col row =
    liftIO $ FFI.duckdb_value_is_null result (fromIntegral col) (fromIntegral row)
