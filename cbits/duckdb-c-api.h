#pragma once

#include "duckdb.h"

#ifdef __cplusplus
extern "C" {
#endif

DUCKDB_API duckdb_data_chunk duckdb_result_get_chunk_capi(duckdb_result *result, idx_t chunk_index);

DUCKDB_API bool duckdb_result_is_streaming_capi(duckdb_result *result);

DUCKDB_API idx_t duckdb_result_chunk_count_capi(duckdb_result *result);

DUCKDB_API duckdb_data_chunk duckdb_stream_fetch_chunk_capi(duckdb_result *result);

#ifdef __cplusplus
}
#endif
