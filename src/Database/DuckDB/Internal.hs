module Database.DuckDB.Internal
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
    )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Database.DuckDB.Internal.FFI (DuckDBState)
import Database.DuckDB.Internal.FFI qualified as FFI
import Foreign.C.String

type DuckDBError = String

isDuckDBError :: DuckDBState -> Bool
isDuckDBError FFI.DuckDBSuccess = False
isDuckDBError _ = True

newtype DuckDBMonad a = DuckDBMonad {runDuckDBMonad :: ExceptT DuckDBError IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadError DuckDBError)

liftIOEither :: IO (Either DuckDBError a) -> DuckDBMonad a
liftIOEither = liftEither <=< liftIO

runDuckDB :: DuckDBMonad a -> IO (Either DuckDBError a)
runDuckDB = runExceptT . runDuckDBMonad

version :: DuckDBMonad String
version = liftIO $ do
    p <- FFI.duckdb_library_version
    peekCString p
