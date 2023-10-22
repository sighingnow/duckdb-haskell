{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}

module Database.DuckDB.Internal.FFI where

import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

{- FOURMOLU_DISABLE -}

#include "duckdb.h"
#include "duckdb-c-api.h"

{- FOURMOLU_ENABLE -}

type DuckDBIndexType = Word64

type DuckDBType = Int32

pattern DUCKDB_TYPE_INVALID :: DuckDBType
pattern DUCKDB_TYPE_INVALID = 0

pattern DUCKDB_TYPE_BOOLEAN :: DuckDBType
pattern DUCKDB_TYPE_BOOLEAN = 1

pattern DUCKDB_TYPE_TINYINT :: DuckDBType
pattern DUCKDB_TYPE_TINYINT = 2

pattern DUCKDB_TYPE_SMALLINT :: DuckDBType
pattern DUCKDB_TYPE_SMALLINT = 3

pattern DUCKDB_TYPE_INTEGER :: DuckDBType
pattern DUCKDB_TYPE_INTEGER = 4

pattern DUCKDB_TYPE_BIGINT :: DuckDBType
pattern DUCKDB_TYPE_BIGINT = 5

pattern DUCKDB_TYPE_UTINYINT :: DuckDBType
pattern DUCKDB_TYPE_UTINYINT = 6

pattern DUCKDB_TYPE_USMALLINT :: DuckDBType
pattern DUCKDB_TYPE_USMALLINT = 7

pattern DUCKDB_TYPE_UINTEGER :: DuckDBType
pattern DUCKDB_TYPE_UINTEGER = 8

pattern DUCKDB_TYPE_UBIGINT :: DuckDBType
pattern DUCKDB_TYPE_UBIGINT = 9

pattern DUCKDB_TYPE_FLOAT :: DuckDBType
pattern DUCKDB_TYPE_FLOAT = 10

pattern DUCKDB_TYPE_DOUBLE :: DuckDBType
pattern DUCKDB_TYPE_DOUBLE = 11

pattern DUCKDB_TYPE_TIMESTAMP :: DuckDBType
pattern DUCKDB_TYPE_TIMESTAMP = 12

pattern DUCKDB_TYPE_DATE :: DuckDBType
pattern DUCKDB_TYPE_DATE = 13

pattern DUCKDB_TYPE_TIME :: DuckDBType
pattern DUCKDB_TYPE_TIME = 14

pattern DUCKDB_TYPE_INTERVAL :: DuckDBType
pattern DUCKDB_TYPE_INTERVAL = 15

pattern DUCKDB_TYPE_HUGEINT :: DuckDBType
pattern DUCKDB_TYPE_HUGEINT = 16

pattern DUCKDB_TYPE_VARCHAR :: DuckDBType
pattern DUCKDB_TYPE_VARCHAR = 17

pattern DUCKDB_TYPE_BLOB :: DuckDBType
pattern DUCKDB_TYPE_BLOB = 18

pattern DUCKDB_TYPE_DECIMAL :: DuckDBType
pattern DUCKDB_TYPE_DECIMAL = 19

pattern DUCKDB_TYPE_TIMESTAMP_S :: DuckDBType
pattern DUCKDB_TYPE_TIMESTAMP_S = 20

pattern DUCKDB_TYPE_TIMESTAMP_MS :: DuckDBType
pattern DUCKDB_TYPE_TIMESTAMP_MS = 21

pattern DUCKDB_TYPE_TIMESTAMP_NS :: DuckDBType
pattern DUCKDB_TYPE_TIMESTAMP_NS = 22

pattern DUCKDB_TYPE_ENUM :: DuckDBType
pattern DUCKDB_TYPE_ENUM = 23

pattern DUCKDB_TYPE_LIST :: DuckDBType
pattern DUCKDB_TYPE_LIST = 24

pattern DUCKDB_TYPE_STRUCT :: DuckDBType
pattern DUCKDB_TYPE_STRUCT = 25

pattern DUCKDB_TYPE_MAP :: DuckDBType
pattern DUCKDB_TYPE_MAP = 26

pattern DUCKDB_TYPE_UUID :: DuckDBType
pattern DUCKDB_TYPE_UUID = 27

pattern DUCKDB_TYPE_UNION :: DuckDBType
pattern DUCKDB_TYPE_UNION = 28

pattern DUCKDB_TYPE_BIT :: DuckDBType
pattern DUCKDB_TYPE_BIT = 29

{-# COMPLETE
    DUCKDB_TYPE_INVALID
    , DUCKDB_TYPE_BOOLEAN
    , DUCKDB_TYPE_TINYINT
    , DUCKDB_TYPE_SMALLINT
    , DUCKDB_TYPE_INTEGER
    , DUCKDB_TYPE_BIGINT
    , DUCKDB_TYPE_UTINYINT
    , DUCKDB_TYPE_USMALLINT
    , DUCKDB_TYPE_UINTEGER
    , DUCKDB_TYPE_UBIGINT
    , DUCKDB_TYPE_FLOAT
    , DUCKDB_TYPE_DOUBLE
    , DUCKDB_TYPE_TIMESTAMP
    , DUCKDB_TYPE_DATE
    , DUCKDB_TYPE_TIME
    , DUCKDB_TYPE_INTERVAL
    , DUCKDB_TYPE_HUGEINT
    , DUCKDB_TYPE_VARCHAR
    , DUCKDB_TYPE_BLOB
    , DUCKDB_TYPE_DECIMAL
    , DUCKDB_TYPE_TIMESTAMP_S
    , DUCKDB_TYPE_TIMESTAMP_MS
    , DUCKDB_TYPE_TIMESTAMP_NS
    , DUCKDB_TYPE_ENUM
    , DUCKDB_TYPE_LIST
    , DUCKDB_TYPE_STRUCT
    , DUCKDB_TYPE_MAP
    , DUCKDB_TYPE_UUID
    , DUCKDB_TYPE_UNION
    , DUCKDB_TYPE_BIT ::
        DuckDBType
    #-}

type DuckDBDate = Int32 -- days

data DuckDBDateStruct = DuckDBDateStruct
    { year :: Int32
    , month :: Int8
    , day :: Int8
    }
    deriving (Eq, Show)

{- FOURMOLU_DISABLE -}

instance Storable DuckDBDateStruct where
    sizeOf _ = (#size duckdb_date_struct)
    alignment _ = alignment (undefined :: CInt)
    peek ptr =
        DuckDBDateStruct
            <$> (#peek duckdb_date_struct, year) ptr
            <*> (#peek duckdb_date_struct, month) ptr
            <*> (#peek duckdb_date_struct, day) ptr
    poke ptr (DuckDBDateStruct year month day) = do
        (#poke duckdb_date_struct, year) ptr year
        (#poke duckdb_date_struct, month) ptr month
        (#poke duckdb_date_struct, day) ptr day

{- FOURMOLU_ENABLE -}

type DuckDBTime = Int64 -- microseconds

data DuckDBTimeStruct = DuckDBTimeStruct
    { hour :: Int8
    , min :: Int8
    , sec :: Int8
    , micros :: Int32
    }
    deriving (Eq, Show)

{- FOURMOLU_DISABLE -}

instance Storable DuckDBTimeStruct where
    sizeOf _ = (#size duckdb_time_struct)
    alignment _ = alignment (undefined :: CInt)
    peek ptr =
        DuckDBTimeStruct
            <$> (#peek duckdb_time_struct, hour) ptr
            <*> (#peek duckdb_time_struct, min) ptr
            <*> (#peek duckdb_time_struct, sec) ptr
            <*> (#peek duckdb_time_struct, micros) ptr
    poke ptr (DuckDBTimeStruct hour minute sec micros) = do
        (#poke duckdb_time_struct, hour) ptr hour
        (#poke duckdb_time_struct, min) ptr minute
        (#poke duckdb_time_struct, sec) ptr sec
        (#poke duckdb_time_struct, micros) ptr micros

{- FOURMOLU_ENABLE -}

type DuckDBTimestamp = Int64 -- microseconds

data DuckDBTimestampStruct = DuckDBTimestampStruct
    { date :: DuckDBDateStruct
    , time :: DuckDBTimeStruct
    }
    deriving (Eq, Show)

{- FOURMOLU_DISABLE -}

instance Storable DuckDBTimestampStruct where
    sizeOf _ = (#size duckdb_timestamp_struct)
    alignment _ = alignment (undefined :: CInt)
    peek ptr =
        DuckDBTimestampStruct
            <$> (#peek duckdb_timestamp_struct, date) ptr
            <*> (#peek duckdb_timestamp_struct, time) ptr
    poke ptr (DuckDBTimestampStruct date time) = do
        (#poke duckdb_timestamp_struct, date) ptr date
        (#poke duckdb_timestamp_struct, time) ptr time

{- FOURMOLU_ENABLE -}

data DuckDBIntervalStruct = DuckDBIntervalStruct
    { months :: Int32
    , days :: Int32
    , micros :: Int64
    }
    deriving (Eq, Show)

{- FOURMOLU_DISABLE -}

instance Storable DuckDBIntervalStruct where
    sizeOf _ = (#size duckdb_interval)
    alignment _ = alignment (undefined :: CInt)
    peek ptr =
        DuckDBIntervalStruct
            <$> (#peek duckdb_interval, months) ptr
            <*> (#peek duckdb_interval, days) ptr
            <*> (#peek duckdb_interval, micros) ptr
    poke ptr (DuckDBIntervalStruct months days micros) = do
        (#poke duckdb_interval, months) ptr months
        (#poke duckdb_interval, days) ptr days
        (#poke duckdb_interval, micros) ptr micros

{- FOURMOLU_ENABLE -}

data DuckDBHugeIntStruct = DuckDBHugeIntStruct
    { lower :: Word64
    , upper :: Int64
    }
    deriving (Eq, Show)

{- FOURMOLU_DISABLE -}

instance Storable DuckDBHugeIntStruct where
    sizeOf _ = (#size duckdb_hugeint)
    alignment _ = alignment (undefined :: CInt)
    peek ptr =
        DuckDBHugeIntStruct
            <$> (#peek duckdb_hugeint, lower) ptr
            <*> (#peek duckdb_hugeint, upper) ptr
    poke ptr (DuckDBHugeIntStruct lower upper) = do
        (#poke duckdb_hugeint, lower) ptr lower
        (#poke duckdb_hugeint, upper) ptr upper

{- FOURMOLU_ENABLE -}

data DuckDBDecimalStruct = DuckDBDecimalStruct
    { width :: Word8
    , scale :: Word8
    , value :: DuckDBHugeIntStruct
    }
    deriving (Eq, Show)

{- FOURMOLU_DISABLE -}

instance Storable DuckDBDecimalStruct where
    sizeOf _ = (#size duckdb_decimal)
    alignment _ = alignment (undefined :: CInt)
    peek ptr =
        DuckDBDecimalStruct
            <$> (#peek duckdb_decimal, width) ptr
            <*> (#peek duckdb_decimal, scale) ptr
            <*> (#peek duckdb_decimal, value) ptr
    poke ptr (DuckDBDecimalStruct width scale value) = do
        (#poke duckdb_decimal, width) ptr width
        (#poke duckdb_decimal, scale) ptr scale
        (#poke duckdb_decimal, value) ptr value

{- FOURMOLU_ENABLE -}

data DuckDBStringStruct = DuckDBStringStruct
    { data_ :: CString
    , size :: DuckDBIndexType
    }
    deriving (Eq, Show)

{- FOURMOLU_DISABLE -}

instance Storable DuckDBStringStruct where
    sizeOf _ = (#size duckdb_string)
    alignment _ = alignment (undefined :: CInt)
    peek ptr =
        DuckDBStringStruct
            <$> (#peek duckdb_string, data) ptr
            <*> (#peek duckdb_string, size) ptr
    poke ptr (DuckDBStringStruct data_ size) = do
        (#poke duckdb_string, data) ptr data_
        (#poke duckdb_string, size) ptr size

{- FOURMOLU_ENABLE -}

-- typedef struct {
-- 	union {
-- 		struct {
-- 			uint32_t length;
-- 			char prefix[4];
-- 			char *ptr;
-- 		} pointer;
-- 		struct {
-- 			uint32_t length;
-- 			char inlined[12];
-- 		} inlined;
-- 	} value;
-- } duckdb_string_t;

data DuckDBBlob = DuckDBBlob
    { data_ :: Ptr ()
    , size :: DuckDBIndexType
    }
    deriving (Eq, Show)

{- FOURMOLU_DISABLE -}

instance Storable DuckDBBlob where
    sizeOf _ = (#size duckdb_blob)
    alignment _ = alignment (undefined :: CInt)
    peek ptr =
        DuckDBBlob
            <$> (#peek duckdb_blob, data) ptr
            <*> (#peek duckdb_blob, size) ptr
    poke ptr (DuckDBBlob data_ size) = do
        (#poke duckdb_blob, data) ptr data_
        (#poke duckdb_blob, size) ptr size

{- FOURMOLU_ENABLE -}

data DuckDBListEntry = DuckDBListEntry
    { offset :: Word64
    , length :: Word64
    }
    deriving (Eq, Show)

{- FOURMOLU_DISABLE -}

instance Storable DuckDBListEntry where
    sizeOf _ = (#size duckdb_list_entry)
    alignment _ = alignment (undefined :: CInt)
    peek ptr =
        DuckDBListEntry
            <$> (#peek duckdb_list_entry, offset) ptr
            <*> (#peek duckdb_list_entry, length) ptr
    poke ptr (DuckDBListEntry offset length_) = do
        (#poke duckdb_list_entry, offset) ptr offset
        (#poke duckdb_list_entry, length) ptr length_

{- FOURMOLU_ENABLE -}

data DuckDBColumn = DuckDBColumn
    { data_ :: Ptr ()
    , nullmask :: Ptr Bool
    , type_ :: DuckDBType
    , name :: CString
    , internal_data :: Ptr ()
    }
    deriving (Eq, Show)

{- FOURMOLU_DISABLE -}

instance Storable DuckDBColumn where
    sizeOf _ = (#size duckdb_column)
    alignment _ = alignment (undefined :: CInt)
    peek ptr =
        DuckDBColumn
            <$> (#peek duckdb_column, __deprecated_data) ptr
            <*> (#peek duckdb_column, __deprecated_nullmask) ptr
            <*> (#peek duckdb_column, __deprecated_type) ptr
            <*> (#peek duckdb_column, __deprecated_name) ptr
            <*> (#peek duckdb_column, internal_data) ptr
    poke ptr (DuckDBColumn data_ nullmask type_ name internal_data) = do
        (#poke duckdb_column, __deprecated_data) ptr data_
        (#poke duckdb_column, __deprecated_nullmask) ptr nullmask
        (#poke duckdb_column, __deprecated_type) ptr type_
        (#poke duckdb_column, __deprecated_name) ptr name
        (#poke duckdb_column, internal_data) ptr internal_data

{- FOURMOLU_ENABLE -}

data DuckDBResult = DuckDBResult
    { column_count :: DuckDBIndexType
    , row_count :: DuckDBIndexType
    , rows_changed :: DuckDBIndexType
    , columns :: Ptr DuckDBColumn
    , error_message :: CString
    , internal_data :: Ptr ()
    }
    deriving (Eq, Show)

{- FOURMOLU_DISABLE -}

instance Storable DuckDBResult where
    sizeOf _ = (#size duckdb_result)
    alignment _ = alignment (undefined :: CInt)
    peek ptr =
        DuckDBResult
            <$> (#peek duckdb_result, __deprecated_column_count) ptr
            <*> (#peek duckdb_result, __deprecated_row_count) ptr
            <*> (#peek duckdb_result, __deprecated_rows_changed) ptr
            <*> (#peek duckdb_result, __deprecated_columns) ptr
            <*> (#peek duckdb_result, __deprecated_error_message) ptr
            <*> (#peek duckdb_result, internal_data) ptr
    poke ptr (DuckDBResult column_count row_count rows_changed columns error_message internal_data) = do
        (#poke duckdb_result, __deprecated_column_count) ptr column_count
        (#poke duckdb_result, __deprecated_row_count) ptr row_count
        (#poke duckdb_result, __deprecated_rows_changed) ptr rows_changed
        (#poke duckdb_result, __deprecated_columns) ptr columns
        (#poke duckdb_result, __deprecated_error_message) ptr error_message
        (#poke duckdb_result, internal_data) ptr internal_data

{- FOURMOLU_ENABLE -}

newtype DuckDBDatabase = DuckDBDatabase (Ptr ()) deriving (Eq, Storable)

newtype DuckDBConnection = DuckDBConnection (Ptr ()) deriving (Eq, Storable)

newtype DuckDBPreparedStatement = DuckDBPreparedStatement (Ptr ())
    deriving (Eq, Storable)

newtype DuckDBExtractedStatements = DuckDBExtractedStatements (Ptr ())
    deriving (Eq, Storable)

newtype DuckDBPendingResult = DuckDBPendingResult (Ptr ())
    deriving (Eq, Storable)

newtype DuckDBAppender = DuckDBAppender (Ptr ()) deriving (Eq, Storable)

newtype DuckDBArrow = DuckDBArrow (Ptr ()) deriving (Eq, Storable)

newtype DuckDBConfig = DuckDBConfig (Ptr ()) deriving (Eq, Storable)

newtype DuckDBArrowSchema = DuckDBArrowSchema (Ptr ()) deriving (Eq, Storable)

newtype DuckDBArrowArray = DuckDBArrowArray (Ptr ()) deriving (Eq, Storable)

newtype DuckDBLogicalType = DuckDBLogicalType (Ptr ()) deriving (Eq, Storable)

newtype DuckDBDataChunk = DuckDBDataChunk (Ptr ()) deriving (Eq, Storable)

newtype DuckDBVector = DuckDBVector (Ptr ()) deriving (Eq, Storable)

newtype DuckDBValue = DuckDBValue (Ptr ()) deriving (Eq, Storable)

type DuckDBState = Int32

pattern DuckDBSuccess :: DuckDBState
pattern DuckDBSuccess = 0

pattern DuckDBError :: DuckDBState
pattern DuckDBError = 1

{-# COMPLETE
    DuckDBSuccess
    , DuckDBError ::
        DuckDBState
    #-}

type DuckDBPendingState = Int32

pattern DUCKDB_PENDING_RESULT_READY :: DuckDBPendingState
pattern DUCKDB_PENDING_RESULT_READY = 0

pattern DUCKDB_PENDING_RESULT_NOT_READY :: DuckDBPendingState
pattern DUCKDB_PENDING_RESULT_NOT_READY = 1

pattern DUCKDB_PENDING_ERROR :: DuckDBPendingState
pattern DUCKDB_PENDING_ERROR = 2

{-# COMPLETE
    DUCKDB_PENDING_RESULT_READY
    , DUCKDB_PENDING_RESULT_NOT_READY
    , DUCKDB_PENDING_ERROR ::
        DuckDBPendingState
    #-}

-----------------------------------------------------
-- Open/Connect
-----------------------------------------------------

foreign import ccall "duckdb.h duckdb_open"
    duckdb_open
        :: CString -> Ptr DuckDBDatabase -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_open_ext"
    duckdb_open_ext
        :: CString -> Ptr DuckDBDatabase -> DuckDBConfig -> Ptr CString -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_close"
    duckdb_close
        :: Ptr DuckDBDatabase -> IO ()

foreign import ccall "duckdb.h duckdb_connect"
    duckdb_connect
        :: DuckDBDatabase -> Ptr DuckDBConnection -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_disconnect"
    duckdb_disconnect
        :: Ptr DuckDBConnection -> IO ()

foreign import ccall "duckdb.h duckdb_library_version"
    duckdb_library_version
        :: IO CString

-----------------------------------------------------
-- Configuration
-----------------------------------------------------

foreign import ccall "duckdb.h duckdb_create_config"
    duckdb_create_config
        :: Ptr DuckDBConfig -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_config_count"
    duckdb_config_count
        :: IO CSize

foreign import ccall "duckdb.h duckdb_get_config_flag"
    duckdb_get_config_flag
        :: CSize -> Ptr CString -> Ptr CString -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_set_config"
    duckdb_set_config
        :: DuckDBConfig -> CString -> CString -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_destroy_config"
    duckdb_destroy_config
        :: Ptr DuckDBConfig -> IO ()

-----------------------------------------------------
-- Query Execution
-----------------------------------------------------

foreign import ccall "duckdb.h duckdb_query"
    duckdb_query
        :: DuckDBConnection -> CString -> Ptr DuckDBResult -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_destroy_result"
    duckdb_destroy_result
        :: Ptr DuckDBResult -> IO ()

foreign import ccall "duckdb.h duckdb_column_name"
    duckdb_column_name
        :: Ptr DuckDBResult -> DuckDBIndexType -> IO CString

foreign import ccall "duckdb.h duckdb_column_type"
    duckdb_column_type
        :: Ptr DuckDBResult -> DuckDBIndexType -> IO DuckDBType

foreign import ccall "duckdb.h duckdb_column_logical_type"
    duckdb_column_logical_type
        :: Ptr DuckDBResult -> DuckDBIndexType -> IO DuckDBLogicalType

foreign import ccall "duckdb.h duckdb_column_count"
    duckdb_column_count
        :: Ptr DuckDBResult -> IO DuckDBIndexType

foreign import ccall "duckdb.h duckdb_row_count"
    duckdb_row_count
        :: Ptr DuckDBResult -> IO DuckDBIndexType

foreign import ccall "duckdb.h duckdb_rows_changed"
    duckdb_rows_changed
        :: Ptr DuckDBResult -> IO DuckDBIndexType

foreign import ccall "duckdb.h duckdb_column_data"
    duckdb_column_data
        :: Ptr DuckDBResult -> DuckDBIndexType -> IO (Ptr a)

foreign import ccall "duckdb.h duckdb_nullmask_data"
    duckdb_nullmask_data
        :: Ptr DuckDBResult -> DuckDBIndexType -> IO (Ptr Bool)

foreign import ccall "duckdb.h duckdb_result_error"
    duckdb_result_error
        :: Ptr DuckDBResult -> IO CString

-----------------------------------------------------
-- Result Functions
-----------------------------------------------------

foreign import ccall "duckdb.h duckdb_result_get_chunk_capi"
    duckdb_result_get_chunk
        :: Ptr DuckDBResult -> DuckDBIndexType -> IO DuckDBDataChunk

foreign import ccall "duckdb.h duckdb_result_is_streaming_capi"
    duckdb_result_is_streaming
        :: Ptr DuckDBResult -> IO Bool

foreign import ccall "duckdb.h duckdb_result_chunk_count_capi"
    duckdb_result_chunk_count
        :: Ptr DuckDBResult -> IO DuckDBIndexType

foreign import ccall "duckdb.h duckdb_value_boolean"
    duckdb_value_boolean
        :: Ptr DuckDBResult -> DuckDBIndexType -> DuckDBIndexType -> IO Bool

foreign import ccall "duckdb.h duckdb_value_int8"
    duckdb_value_int8
        :: Ptr DuckDBResult -> DuckDBIndexType -> DuckDBIndexType -> IO Int8

foreign import ccall "duckdb.h duckdb_value_int16"
    duckdb_value_int16
        :: Ptr DuckDBResult -> DuckDBIndexType -> DuckDBIndexType -> IO Int16

foreign import ccall "duckdb.h duckdb_value_int32"
    duckdb_value_int32
        :: Ptr DuckDBResult -> DuckDBIndexType -> DuckDBIndexType -> IO Int32

foreign import ccall "duckdb.h duckdb_value_int64"
    duckdb_value_int64
        :: Ptr DuckDBResult -> DuckDBIndexType -> DuckDBIndexType -> IO Int64

-- DUCKDB_API duckdb_hugeint duckdb_value_hugeint(duckdb_result *result, idx_t col, idx_t row);

-- DUCKDB_API duckdb_decimal duckdb_value_decimal(duckdb_result *result, idx_t col, idx_t row);

foreign import ccall "duckdb.h duckdb_value_uint8"
    duckdb_value_uint8
        :: Ptr DuckDBResult -> DuckDBIndexType -> DuckDBIndexType -> IO Word8

foreign import ccall "duckdb.h duckdb_value_uint16"
    duckdb_value_uint16
        :: Ptr DuckDBResult -> DuckDBIndexType -> DuckDBIndexType -> IO Word16

foreign import ccall "duckdb.h duckdb_value_uint32"
    duckdb_value_uint32
        :: Ptr DuckDBResult -> DuckDBIndexType -> DuckDBIndexType -> IO Word32

foreign import ccall "duckdb.h duckdb_value_uint64"
    duckdb_value_uint64
        :: Ptr DuckDBResult -> DuckDBIndexType -> DuckDBIndexType -> IO Word64

foreign import ccall "duckdb.h duckdb_value_float"
    duckdb_value_float
        :: Ptr DuckDBResult -> DuckDBIndexType -> DuckDBIndexType -> IO Float

foreign import ccall "duckdb.h duckdb_value_double"
    duckdb_value_double
        :: Ptr DuckDBResult -> DuckDBIndexType -> DuckDBIndexType -> IO Double

foreign import ccall "duckdb.h duckdb_value_date"
    duckdb_value_date
        :: Ptr DuckDBResult -> DuckDBIndexType -> DuckDBIndexType -> IO DuckDBDate

foreign import ccall "duckdb.h duckdb_value_time"
    duckdb_value_time
        :: Ptr DuckDBResult -> DuckDBIndexType -> DuckDBIndexType -> IO DuckDBTime

foreign import ccall "duckdb.h duckdb_value_timestamp"
    duckdb_value_timestamp
        :: Ptr DuckDBResult -> DuckDBIndexType -> DuckDBIndexType -> IO DuckDBTimestamp

-- DUCKDB_API duckdb_interval duckdb_value_interval(duckdb_result *result, idx_t col, idx_t row);

foreign import ccall "duckdb.h duckdb_value_varchar"
    duckdb_value_varchar
        :: Ptr DuckDBResult -> DuckDBIndexType -> DuckDBIndexType -> IO CString

-- DUCKDB_API duckdb_string duckdb_value_string(duckdb_result *result, idx_t col, idx_t row);

foreign import ccall "duckdb.h duckdb_value_varchar_internal"
    duckdb_value_varchar_internal
        :: Ptr DuckDBResult -> DuckDBIndexType -> DuckDBIndexType -> IO CString

-- DUCKDB_API duckdb_string duckdb_value_string_internal(duckdb_result *result, idx_t col, idx_t row);

-- DUCKDB_API duckdb_blob duckdb_value_blob(duckdb_result *result, idx_t col, idx_t row);

foreign import ccall "duckdb.h duckdb_value_is_null"
    duckdb_value_is_null
        :: Ptr DuckDBResult -> DuckDBIndexType -> DuckDBIndexType -> IO Bool

-----------------------------------------------------
-- Helpers
-----------------------------------------------------

foreign import ccall "duckdb.h duckdb_malloc"
    duckdb_malloc
        :: CSize -> IO (Ptr a)

foreign import ccall "duckdb.h duckdb_free"
    duckdb_free
        :: Ptr a -> IO ()

foreign import ccall "duckdb.h duckdb_vector_size"
    duckdb_vector_size
        :: IO DuckDBIndexType

-- DUCKDB_API bool duckdb_string_is_inlined(duckdb_string_t string);

-----------------------------------------------------
-- Date/Time/Timestamp Helpers
-----------------------------------------------------

-- DUCKDB_API duckdb_date_struct duckdb_from_date(duckdb_date date);

-- DUCKDB_API duckdb_date duckdb_to_date(duckdb_date_struct date);

-- DUCKDB_API duckdb_time_struct duckdb_from_time(duckdb_time time);

-- DUCKDB_API duckdb_time duckdb_to_time(duckdb_time_struct time);

-- DUCKDB_API duckdb_timestamp_struct duckdb_from_timestamp(duckdb_timestamp ts);

-- DUCKDB_API duckdb_timestamp duckdb_to_timestamp(duckdb_timestamp_struct ts);

-----------------------------------------------------
-- Hugeint Helpers
-----------------------------------------------------

-- DUCKDB_API double duckdb_hugeint_to_double(duckdb_hugeint val);

-- DUCKDB_API duckdb_hugeint duckdb_double_to_hugeint(double val);

-----------------------------------------------------
-- Decimal Helpers
-----------------------------------------------------

-- DUCKDB_API duckdb_decimal duckdb_double_to_decimal(double val, uint8_t width, uint8_t scale);

-- DUCKDB_API double duckdb_decimal_to_double(duckdb_decimal val);

-----------------------------------------------------
-- Prepared Statements
-----------------------------------------------------

foreign import ccall "duckdb.h duckdb_prepare"
    duckdb_prepare
        :: DuckDBConnection -> CString -> Ptr DuckDBPreparedStatement -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_destroy_prepare"
    duckdb_destroy_prepare
        :: Ptr DuckDBPreparedStatement -> IO ()

foreign import ccall "duckdb.h duckdb_prepare_error"
    duckdb_prepare_error
        :: DuckDBPreparedStatement -> IO CString

foreign import ccall "duckdb.h duckdb_nparams"
    duckdb_nparams
        :: DuckDBPreparedStatement -> IO DuckDBIndexType

foreign import ccall "duckdb.h duckdb_param_type"
    duckdb_param_type
        :: DuckDBPreparedStatement -> DuckDBIndexType -> IO DuckDBType

foreign import ccall "duckdb.h duckdb_clear_bindings"
    duckdb_clear_bindings
        :: DuckDBPreparedStatement -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_bind_boolean"
    duckdb_bind_boolean
        :: DuckDBPreparedStatement -> DuckDBIndexType -> Bool -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_bind_int8"
    duckdb_bind_int8
        :: DuckDBPreparedStatement -> DuckDBIndexType -> Int8 -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_bind_int16"
    duckdb_bind_int16
        :: DuckDBPreparedStatement -> DuckDBIndexType -> Int16 -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_bind_int32"
    duckdb_bind_int32
        :: DuckDBPreparedStatement -> DuckDBIndexType -> Int32 -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_bind_int64"
    duckdb_bind_int64
        :: DuckDBPreparedStatement -> DuckDBIndexType -> Int64 -> IO DuckDBState

-- DUCKDB_API duckdb_state duckdb_bind_hugeint(duckdb_prepared_statement prepared_statement, idx_t param_idx,
--                                             duckdb_hugeint val);

-- DUCKDB_API duckdb_state duckdb_bind_decimal(duckdb_prepared_statement prepared_statement, idx_t param_idx,
--                                             duckdb_decimal val);

foreign import ccall "duckdb.h duckdb_bind_uint8"
    duckdb_bind_uint8
        :: DuckDBPreparedStatement -> DuckDBIndexType -> Word8 -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_bind_uint16"
    duckdb_bind_uint16
        :: DuckDBPreparedStatement -> DuckDBIndexType -> Word16 -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_bind_uint32"
    duckdb_bind_uint32
        :: DuckDBPreparedStatement -> DuckDBIndexType -> Word32 -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_bind_uint64"
    duckdb_bind_uint64
        :: DuckDBPreparedStatement -> DuckDBIndexType -> Word64 -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_bind_float"
    duckdb_bind_float
        :: DuckDBPreparedStatement -> DuckDBIndexType -> Float -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_bind_double"
    duckdb_bind_double
        :: DuckDBPreparedStatement -> DuckDBIndexType -> Double -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_bind_date"
    duckdb_bind_date
        :: DuckDBPreparedStatement -> DuckDBIndexType -> DuckDBDate -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_bind_time"
    duckdb_bind_time
        :: DuckDBPreparedStatement -> DuckDBIndexType -> DuckDBTime -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_bind_timestamp"
    duckdb_bind_timestamp
        :: DuckDBPreparedStatement -> DuckDBIndexType -> DuckDBTimestamp -> IO DuckDBState

-- DUCKDB_API duckdb_state duckdb_bind_interval(duckdb_prepared_statement prepared_statement, idx_t param_idx,
--                                              duckdb_interval val);

foreign import ccall "duckdb.h duckdb_bind_varchar"
    duckdb_bind_varchar
        :: DuckDBPreparedStatement -> DuckDBIndexType -> CString -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_bind_varchar_length"
    duckdb_bind_varchar_length
        :: DuckDBPreparedStatement
        -> DuckDBIndexType
        -> CString
        -> DuckDBIndexType
        -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_bind_blob"
    duckdb_bind_blob
        :: DuckDBPreparedStatement
        -> DuckDBIndexType
        -> Ptr ()
        -> DuckDBIndexType
        -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_bind_null"
    duckdb_bind_null
        :: DuckDBPreparedStatement -> DuckDBIndexType -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_execute_prepared"
    duckdb_execute_prepared
        :: DuckDBPreparedStatement -> Ptr DuckDBResult -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_execute_prepared_arrow"
    duckdb_execute_prepared_arrow
        :: DuckDBPreparedStatement -> Ptr DuckDBArrow -> IO DuckDBState

-----------------------------------------------------
-- Extract Statements
-----------------------------------------------------

foreign import ccall "duckdb.h duckdb_extract_statements"
    duckdb_extract_statements
        :: DuckDBConnection
        -> CString
        -> Ptr DuckDBExtractedStatements
        -> IO DuckDBIndexType

foreign import ccall "duckdb.h duckdb_prepare_extracted_statement"
    duckdb_prepare_extracted_statement
        :: DuckDBConnection
        -> DuckDBExtractedStatements
        -> DuckDBIndexType
        -> Ptr DuckDBPreparedStatement
        -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_extract_statements_error"
    duckdb_extract_statements_error
        :: DuckDBExtractedStatements -> IO CString

foreign import ccall "duckdb.h duckdb_destroy_extracted"
    duckdb_destroy_extracted
        :: Ptr DuckDBExtractedStatements -> IO ()

-----------------------------------------------------
-- Pending Result Interface
-----------------------------------------------------

foreign import ccall "duckdb.h duckdb_pending_prepared"
    duckdb_pending_prepared
        :: DuckDBPreparedStatement -> Ptr DuckDBPendingResult -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_pending_prepared_streaming"
    duckdb_pending_prepared_streaming
        :: DuckDBPreparedStatement -> Ptr DuckDBPendingResult -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_destroy_pending"
    duckdb_destroy_pending
        :: Ptr DuckDBPendingResult -> IO ()

foreign import ccall "duckdb.h duckdb_pending_error"
    duckdb_pending_error
        :: DuckDBPendingResult -> IO CString

foreign import ccall "duckdb.h duckdb_pending_execute_task"
    duckdb_pending_execute_task
        :: DuckDBPendingResult -> IO DuckDBPendingState

foreign import ccall "duckdb.h duckdb_execute_pending"
    duckdb_execute_pending
        :: DuckDBPendingResult -> Ptr DuckDBResult -> IO DuckDBState

-----------------------------------------------------
-- Value Interface
-----------------------------------------------------

foreign import ccall "duckdb.h duckdb_destroy_value"
    duckdb_destroy_value
        :: Ptr DuckDBValue -> IO ()

foreign import ccall "duckdb.h duckdb_create_varchar"
    duckdb_create_varchar
        :: CString -> IO DuckDBValue

foreign import ccall "duckdb.h duckdb_create_varchar_length"
    duckdb_create_varchar_length
        :: CString -> DuckDBIndexType -> IO DuckDBValue

foreign import ccall "duckdb.h duckdb_create_int64"
    duckdb_create_int64
        :: Int64 -> IO DuckDBValue

foreign import ccall "duckdb.h duckdb_get_varchar"
    duckdb_get_varchar
        :: DuckDBValue -> IO CString

foreign import ccall "duckdb.h duckdb_get_int64"
    duckdb_get_int64
        :: DuckDBValue -> IO Int64

-----------------------------------------------------
-- Logical Type Interface
-----------------------------------------------------

foreign import ccall "duckdb.h duckdb_create_logical_type"
    duckdb_create_logical_type
        :: DuckDBType -> IO DuckDBLogicalType

foreign import ccall "duckdb.h duckdb_create_list_type"
    duckdb_create_list_type
        :: DuckDBLogicalType -> IO DuckDBLogicalType

foreign import ccall "duckdb.h duckdb_create_map_type"
    duckdb_create_map_type
        :: DuckDBLogicalType -> DuckDBLogicalType -> IO DuckDBLogicalType

foreign import ccall "duckdb.h duckdb_create_union_type"
    duckdb_create_union_type
        :: Ptr DuckDBLogicalType -> Ptr CString -> DuckDBIndexType -> IO DuckDBLogicalType

foreign import ccall "duckdb.h duckdb_create_decimal_type"
    duckdb_create_decimal_type
        :: Word8 -> Word8 -> IO DuckDBLogicalType

foreign import ccall "duckdb.h duckdb_get_type_id"
    duckdb_get_type_id
        :: DuckDBLogicalType -> IO DuckDBType

foreign import ccall "duckdb.h duckdb_decimal_width"
    duckdb_decimal_width
        :: DuckDBLogicalType -> IO Word8

foreign import ccall "duckdb.h duckdb_decimal_scale"
    duckdb_decimal_scale
        :: DuckDBLogicalType -> IO Word8

foreign import ccall "duckdb.h duckdb_decimal_internal_type"
    duckdb_decimal_internal_type
        :: DuckDBLogicalType -> IO DuckDBType

foreign import ccall "duckdb.h duckdb_enum_internal_type"
    duckdb_enum_internal_type
        :: DuckDBLogicalType -> IO DuckDBType

foreign import ccall "duckdb.h duckdb_enum_dictionary_size"
    duckdb_enum_dictionary_size
        :: DuckDBLogicalType -> IO Word32

foreign import ccall "duckdb.h duckdb_enum_dictionary_value"
    duckdb_enum_dictionary_value
        :: DuckDBLogicalType -> DuckDBIndexType -> IO CString

foreign import ccall "duckdb.h duckdb_list_type_child_type"
    duckdb_list_type_child_type
        :: DuckDBLogicalType -> IO DuckDBLogicalType

foreign import ccall "duckdb.h duckdb_map_type_key_type"
    duckdb_map_type_key_type
        :: DuckDBLogicalType -> IO DuckDBLogicalType

foreign import ccall "duckdb.h duckdb_map_type_value_type"
    duckdb_map_type_value_type
        :: DuckDBLogicalType -> IO DuckDBLogicalType

foreign import ccall "duckdb.h duckdb_struct_type_child_count"
    duckdb_struct_type_child_count
        :: DuckDBLogicalType -> IO DuckDBIndexType

foreign import ccall "duckdb.h duckdb_struct_type_child_name"
    duckdb_struct_type_child_name
        :: DuckDBLogicalType -> DuckDBIndexType -> IO CString

foreign import ccall "duckdb.h duckdb_struct_type_child_type"
    duckdb_struct_type_child_type
        :: DuckDBLogicalType -> DuckDBIndexType -> IO DuckDBLogicalType

foreign import ccall "duckdb.h duckdb_union_type_member_count"
    duckdb_union_type_member_count
        :: DuckDBLogicalType -> IO DuckDBIndexType

foreign import ccall "duckdb.h duckdb_union_type_member_name"
    duckdb_union_type_member_name
        :: DuckDBLogicalType -> DuckDBIndexType -> IO CString

foreign import ccall "duckdb.h duckdb_union_type_member_type"
    duckdb_union_type_member_type
        :: DuckDBLogicalType -> DuckDBIndexType -> IO DuckDBLogicalType

foreign import ccall "duckdb.h duckdb_destroy_logical_type"
    duckdb_destroy_logical_type
        :: Ptr DuckDBLogicalType -> IO ()

-----------------------------------------------------
-- Data Chunk Interface
-----------------------------------------------------

foreign import ccall "duckdb.h duckdb_create_data_chunk"
    duckdb_create_data_chunk
        :: Ptr DuckDBLogicalType -> DuckDBIndexType -> IO DuckDBDataChunk

foreign import ccall "duckdb.h duckdb_destroy_data_chunk"
    duckdb_destroy_data_chunk
        :: Ptr DuckDBDataChunk -> IO ()

foreign import ccall "duckdb.h duckdb_data_chunk_reset"
    duckdb_data_chunk_reset
        :: DuckDBDataChunk -> IO ()

foreign import ccall "duckdb.h duckdb_data_chunk_get_column_count"
    duckdb_data_chunk_get_column_count
        :: DuckDBDataChunk -> IO DuckDBIndexType

foreign import ccall "duckdb.h duckdb_data_chunk_get_vector"
    duckdb_data_chunk_get_vector
        :: DuckDBDataChunk -> DuckDBIndexType -> IO DuckDBVector

foreign import ccall "duckdb.h duckdb_data_chunk_get_size"
    duckdb_data_chunk_get_size
        :: DuckDBDataChunk -> IO DuckDBIndexType

foreign import ccall "duckdb.h duckdb_data_chunk_set_size"
    duckdb_data_chunk_set_size
        :: DuckDBDataChunk -> DuckDBIndexType -> IO ()

-----------------------------------------------------
-- Vector Interface
-----------------------------------------------------
foreign import ccall "duckdb.h duckdb_vector_get_column_type"
    duckdb_vector_get_column_type
        :: DuckDBVector -> IO DuckDBLogicalType

foreign import ccall "duckdb.h duckdb_vector_get_data"
    duckdb_vector_get_data
        :: DuckDBVector -> IO (Ptr a)

foreign import ccall "duckdb.h duckdb_vector_get_validity"
    duckdb_vector_get_validity
        :: DuckDBVector -> IO (Ptr Word64)

foreign import ccall "duckdb.h duckdb_vector_ensure_validity_writable"
    duckdb_vector_ensure_validity_writable
        :: DuckDBVector -> IO ()

foreign import ccall "duckdb.h duckdb_vector_assign_string_element"
    duckdb_vector_assign_string_element
        :: DuckDBVector -> DuckDBIndexType -> CString -> IO ()

foreign import ccall "duckdb.h duckdb_vector_assign_string_element_len"
    duckdb_vector_assign_string_element_len
        :: DuckDBVector -> DuckDBIndexType -> CString -> DuckDBIndexType -> IO ()

foreign import ccall "duckdb.h duckdb_list_vector_get_child"
    duckdb_list_vector_get_child
        :: DuckDBVector -> IO DuckDBVector

foreign import ccall "duckdb.h duckdb_list_vector_get_size"
    duckdb_list_vector_get_size
        :: DuckDBVector -> IO DuckDBIndexType

foreign import ccall "duckdb.h duckdb_list_vector_set_size"
    duckdb_list_vector_set_size
        :: DuckDBVector -> DuckDBIndexType -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_list_vector_reserve"
    duckdb_list_vector_reserve
        :: DuckDBVector -> DuckDBIndexType -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_struct_vector_get_child"
    duckdb_struct_vector_get_child
        :: DuckDBVector -> DuckDBIndexType -> IO DuckDBVector

-----------------------------------------------------
-- Validity Mask Functions
-----------------------------------------------------
foreign import ccall "duckdb.h duckdb_validity_row_is_valid"
    duckdb_validity_row_is_valid
        :: Ptr Word64 -> DuckDBIndexType -> IO Bool

foreign import ccall "duckdb.h duckdb_validity_set_row_validity"
    duckdb_validity_set_row_validity
        :: Ptr Word64 -> DuckDBIndexType -> Bool -> IO ()

foreign import ccall "duckdb.h duckdb_validity_set_row_invalid"
    duckdb_validity_set_row_invalid
        :: Ptr Word64 -> DuckDBIndexType -> IO ()

foreign import ccall "duckdb.h duckdb_validity_set_row_valid"
    duckdb_validity_set_row_valid
        :: Ptr Word64 -> DuckDBIndexType -> IO ()

-----------------------------------------------------
-- Table Functions
-----------------------------------------------------

newtype DuckDBTableFunction = DuckDBTableFunction (Ptr ())
    deriving (Eq, Storable)

newtype DuckDBBindInfo = DuckDBBindInfo (Ptr ()) deriving (Eq, Storable)

newtype DuckDBInitInfo = DuckDBInitInfo (Ptr ()) deriving (Eq, Storable)

newtype DuckDBFunctionInfo = DuckDBFunctionInfo (Ptr ()) deriving (Eq, Storable)

type DuckDBTableFunctionBindFunc = FunPtr (DuckDBBindInfo -> IO ())

type DuckDBTableFunctionInitFunc = FunPtr (DuckDBInitInfo -> IO ())

type DuckDBTableFunctionFunc =
    FunPtr (DuckDBFunctionInfo -> DuckDBDataChunk -> IO ())

type DuckDBDeleteCallbackFunc = FunPtr (Ptr () -> IO ())

foreign import ccall "duckdb.h duckdb_create_table_function"
    duckdb_create_table_function
        :: IO DuckDBTableFunction

foreign import ccall "duckdb.h duckdb_destroy_table_function"
    duckdb_destroy_table_function
        :: DuckDBTableFunction -> IO ()

foreign import ccall "duckdb.h duckdb_table_function_set_name"
    duckdb_table_function_set_name
        :: DuckDBTableFunction -> CString -> IO ()

foreign import ccall "duckdb.h duckdb_table_function_add_parameter"
    duckdb_table_function_add_parameter
        :: DuckDBTableFunction -> DuckDBLogicalType -> IO ()

foreign import ccall "duckdb.h duckdb_table_function_add_named_parameter"
    duckdb_table_function_add_named_parameter
        :: DuckDBTableFunction -> CString -> DuckDBLogicalType -> IO ()

foreign import ccall "duckdb.h duckdb_table_function_set_extra_info"
    duckdb_table_function_set_extra_info
        :: DuckDBTableFunction -> Ptr () -> DuckDBDeleteCallbackFunc -> IO ()

foreign import ccall "duckdb.h duckdb_table_function_set_bind"
    duckdb_table_function_set_bind
        :: DuckDBTableFunction -> DuckDBTableFunctionBindFunc -> IO ()

foreign import ccall "duckdb.h duckdb_table_function_set_init"
    duckdb_table_function_set_init
        :: DuckDBTableFunction -> DuckDBTableFunctionInitFunc -> IO ()

foreign import ccall "duckdb.h duckdb_table_function_set_local_init"
    duckdb_table_function_set_local_init
        :: DuckDBTableFunction -> DuckDBTableFunctionInitFunc -> IO ()

foreign import ccall "duckdb.h duckdb_table_function_set_function"
    duckdb_table_function_set_function
        :: DuckDBTableFunction -> DuckDBTableFunctionFunc -> IO ()

foreign import ccall "duckdb.h duckdb_table_function_supports_projection_pushdown"
    duckdb_table_function_supports_projection_pushdown
        :: DuckDBTableFunction -> Bool -> IO ()

foreign import ccall "duckdb.h duckdb_register_table_function"
    duckdb_register_table_function
        :: DuckDBConnection -> DuckDBTableFunction -> IO DuckDBState

-----------------------------------------------------
-- Table Function Bind
-----------------------------------------------------

foreign import ccall "duckdb.h duckdb_bind_get_extra_info"
    duckdb_bind_get_extra_info
        :: DuckDBBindInfo -> IO (Ptr ())

foreign import ccall "duckdb.h duckdb_bind_add_result_column"
    duckdb_bind_add_result_column
        :: DuckDBBindInfo -> CString -> DuckDBLogicalType -> IO ()

foreign import ccall "duckdb.h duckdb_bind_get_parameter_count"
    duckdb_bind_get_parameter_count
        :: DuckDBBindInfo -> IO DuckDBIndexType

foreign import ccall "duckdb.h duckdb_bind_get_parameter"
    duckdb_bind_get_parameter
        :: DuckDBBindInfo -> DuckDBIndexType -> IO DuckDBValue

foreign import ccall "duckdb.h duckdb_bind_get_named_parameter"
    duckdb_bind_get_named_parameter
        :: DuckDBBindInfo -> CString -> IO DuckDBValue

foreign import ccall "duckdb.h duckdb_bind_set_bind_data"
    duckdb_bind_set_bind_data
        :: DuckDBBindInfo -> Ptr () -> DuckDBDeleteCallbackFunc -> IO ()

foreign import ccall "duckdb.h duckdb_bind_set_cardinality"
    duckdb_bind_set_cardinality
        :: DuckDBBindInfo -> DuckDBIndexType -> Bool -> IO ()

foreign import ccall "duckdb.h duckdb_bind_set_error"
    duckdb_bind_set_error
        :: DuckDBBindInfo -> CString -> IO ()

-----------------------------------------------------
-- Table Function Init
-----------------------------------------------------

foreign import ccall "duckdb.h duckdb_init_get_extra_info"
    duckdb_init_get_extra_info
        :: DuckDBInitInfo -> IO (Ptr ())

foreign import ccall "duckdb.h duckdb_init_get_bind_data"
    duckdb_init_get_bind_data
        :: DuckDBInitInfo -> IO (Ptr ())

foreign import ccall "duckdb.h duckdb_init_set_init_data"
    duckdb_init_set_init_data
        :: DuckDBInitInfo -> Ptr () -> DuckDBDeleteCallbackFunc -> IO ()

foreign import ccall "duckdb.h duckdb_init_get_column_count"
    duckdb_init_get_column_count
        :: DuckDBInitInfo -> IO DuckDBIndexType

foreign import ccall "duckdb.h duckdb_init_get_column_index"
    duckdb_init_get_column_index
        :: DuckDBInitInfo -> DuckDBIndexType -> IO DuckDBIndexType

foreign import ccall "duckdb.h duckdb_init_set_max_threads"
    duckdb_init_set_max_threads
        :: DuckDBInitInfo -> DuckDBIndexType -> IO ()

foreign import ccall "duckdb.h duckdb_init_set_error"
    duckdb_init_set_error
        :: DuckDBInitInfo -> CString -> IO ()

-----------------------------------------------------
-- Table Function
-----------------------------------------------------

foreign import ccall "duckdb.h duckdb_function_get_extra_info"
    duckdb_function_get_extra_info
        :: DuckDBFunctionInfo -> IO (Ptr ())

foreign import ccall "duckdb.h duckdb_function_get_bind_data"
    duckdb_function_get_bind_data
        :: DuckDBFunctionInfo -> IO (Ptr ())

foreign import ccall "duckdb.h duckdb_function_get_init_data"
    duckdb_function_get_init_data
        :: DuckDBFunctionInfo -> IO (Ptr ())

foreign import ccall "duckdb.h duckdb_function_get_local_init_data"
    duckdb_function_get_local_init_data
        :: DuckDBFunctionInfo -> IO (Ptr ())

foreign import ccall "duckdb.h duckdb_function_set_error"
    duckdb_function_set_error
        :: DuckDBFunctionInfo -> CString -> IO ()

-----------------------------------------------------
-- Replacement Scans
-----------------------------------------------------

newtype DuckDBReplacementScanInfo = DuckDBReplacementScanInfo (Ptr ())
    deriving (Eq, Storable)

type DuckDBReplacementCallbackFunc =
    FunPtr (DuckDBReplacementScanInfo -> CString -> Ptr () -> IO ())

foreign import ccall "duckdb.h duckdb_add_replacement_scan"
    duckdb_add_replacement_scan
        :: DuckDBDatabase
        -> DuckDBReplacementCallbackFunc
        -> Ptr ()
        -> DuckDBDeleteCallbackFunc
        -> IO ()

foreign import ccall "duckdb.h duckdb_replacement_scan_set_function_name"
    duckdb_replacement_scan_set_function_name
        :: DuckDBReplacementScanInfo -> CString -> IO ()

foreign import ccall "duckdb.h duckdb_replacement_scan_add_parameter"
    duckdb_replacement_scan_add_parameter
        :: DuckDBReplacementScanInfo -> DuckDBValue -> IO ()

foreign import ccall "duckdb.h duckdb_replacement_scan_set_error"
    duckdb_replacement_scan_set_error
        :: DuckDBReplacementScanInfo -> CString -> IO ()

-----------------------------------------------------
-- Appender
-----------------------------------------------------

foreign import ccall "duckdb.h duckdb_appender_create"
    duckdb_appender_create
        :: DuckDBConnection
        -> CString
        -> CString
        -> Ptr DuckDBAppender
        -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_appender_error"
    duckdb_appender_error
        :: DuckDBAppender -> IO CString

foreign import ccall "duckdb.h duckdb_appender_flush"
    duckdb_appender_flush
        :: DuckDBAppender -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_appender_close"
    duckdb_appender_close
        :: DuckDBAppender -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_appender_destroy"
    duckdb_appender_destroy
        :: Ptr DuckDBAppender -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_appender_begin_row"
    duckdb_appender_begin_row :: DuckDBAppender -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_appender_end_row"
    duckdb_appender_end_row :: DuckDBAppender -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_append_bool"
    duckdb_append_bool :: DuckDBAppender -> Bool -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_append_int8"
    duckdb_append_int8 :: DuckDBAppender -> Int8 -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_append_int16"
    duckdb_append_int16 :: DuckDBAppender -> Int16 -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_append_int32"
    duckdb_append_int32 :: DuckDBAppender -> Int32 -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_append_int64"
    duckdb_append_int64 :: DuckDBAppender -> Int64 -> IO DuckDBState

-- DUCKDB_API duckdb_state duckdb_append_hugeint(duckdb_appender appender, duckdb_hugeint value);

foreign import ccall "duckdb.h duckdb_append_uint8"
    duckdb_append_uint8 :: DuckDBAppender -> Word8 -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_append_uint16"
    duckdb_append_uint16 :: DuckDBAppender -> Word16 -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_append_uint32"
    duckdb_append_uint32 :: DuckDBAppender -> Word32 -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_append_uint64"
    duckdb_append_uint64 :: DuckDBAppender -> Word64 -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_append_float"
    duckdb_append_float :: DuckDBAppender -> Float -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_append_double"
    duckdb_append_double :: DuckDBAppender -> Double -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_append_date"
    duckdb_append_date :: DuckDBAppender -> DuckDBDate -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_append_time"
    duckdb_append_time :: DuckDBAppender -> DuckDBTime -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_append_timestamp"
    duckdb_append_timestamp :: DuckDBAppender -> DuckDBTimestamp -> IO DuckDBState

-- DUCKDB_API duckdb_state duckdb_append_interval(duckdb_appender appender, duckdb_interval value);

foreign import ccall "duckdb.h duckdb_append_varchar"
    duckdb_append_varchar :: DuckDBAppender -> CString -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_append_varchar_length"
    duckdb_append_varchar_length
        :: DuckDBAppender -> CString -> DuckDBIndexType -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_append_blob"
    duckdb_append_blob
        :: DuckDBAppender -> Ptr () -> DuckDBIndexType -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_append_null"
    duckdb_append_null :: DuckDBAppender -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_append_data_chunk"
    duckdb_append_data_chunk :: DuckDBAppender -> DuckDBDataChunk -> IO DuckDBState

-----------------------------------------------------
-- Arrow Interface
-----------------------------------------------------

foreign import ccall "duckdb.h duckdb_query_arrow"
    duckdb_query_arrow
        :: DuckDBConnection -> CString -> Ptr DuckDBArrow -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_query_arrow_schema"
    duckdb_query_arrow_schema
        :: DuckDBArrow -> Ptr DuckDBArrowSchema -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_query_arrow_array"
    duckdb_query_arrow_array
        :: DuckDBArrow -> Ptr DuckDBArrowArray -> IO DuckDBState

foreign import ccall "duckdb.h duckdb_arrow_column_count"
    duckdb_arrow_column_count :: DuckDBArrow -> IO DuckDBIndexType

foreign import ccall "duckdb.h duckdb_arrow_row_count"
    duckdb_arrow_row_count :: DuckDBArrow -> IO DuckDBIndexType

foreign import ccall "duckdb.h duckdb_arrow_rows_changed"
    duckdb_arrow_rows_changed :: DuckDBArrow -> IO DuckDBIndexType

foreign import ccall "duckdb.h duckdb_query_arrow_error"
    duckdb_query_arrow_error :: DuckDBArrow -> IO CString

foreign import ccall "duckdb.h duckdb_destroy_arrow"
    duckdb_destroy_arrow :: DuckDBArrow -> IO ()

-----------------------------------------------------
-- Threading Information
-----------------------------------------------------

newtype DuckDBTaskState = DuckDBTaskState (Ptr ()) deriving (Eq, Storable)

foreign import ccall "duckdb.h duckdb_execute_tasks"
    duckdb_execute_tasks :: DuckDBDatabase -> DuckDBIndexType -> IO ()

foreign import ccall "duckdb.h duckdb_create_task_state"
    duckdb_create_task_state :: DuckDBDatabase -> IO DuckDBTaskState

foreign import ccall "duckdb.h duckdb_execute_tasks_state"
    duckdb_execute_tasks_state :: DuckDBTaskState -> IO ()

foreign import ccall "duckdb.h duckdb_execute_n_tasks_state"
    duckdb_execute_n_tasks_state
        :: DuckDBTaskState -> DuckDBIndexType -> IO DuckDBIndexType

foreign import ccall "duckdb.h duckdb_finish_execution"
    duckdb_finish_execution :: DuckDBTaskState -> IO ()

foreign import ccall "duckdb.h duckdb_task_state_is_finished"
    duckdb_task_state_is_finished :: DuckDBTaskState -> IO Bool

foreign import ccall "duckdb.h duckdb_destroy_task_state"
    duckdb_destroy_task_state :: DuckDBTaskState -> IO ()

foreign import ccall "duckdb.h duckdb_execution_is_finished"
    duckdb_execution_is_finished :: DuckDBConnection -> IO Bool

-----------------------------------------------------
-- Streaming Result Interface
-----------------------------------------------------

foreign import ccall "duckdb.h duckdb_stream_fetch_chunk_capi"
    duckdb_stream_fetch_chunk :: Ptr DuckDBResult -> IO DuckDBDataChunk
