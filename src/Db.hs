{-# LANGUAGE OverloadedStrings #-}

module Db
  ( connect,
    getFileRecord,
    insertFileRecord,
    insertFileRecords,
    migrateDb,
    resetDb,
    updateFileRecord,
    DbConnection,
  )
where

import Control.Monad (when)
import Control.Monad.Logger (runNoLoggingT) -- , runStdoutLoggingT)
import Control.Monad.Trans (liftIO)
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.Persist.Sqlite
import Models

-- | Database connection
type DbConnection = ConnectionPool

-- | Connects to database (creating if not exists already).
connect ::
  Bool ->
  String ->
  String ->
  String ->
  IO DbConnection
connect verbose path ps name = do
  let dbPath = path ++ ps ++ name
  when verbose $ liftIO $ putStrLn $ "Connecting to database " ++ dbPath
  runNoLoggingT $ createSqlitePool (T.pack dbPath) 1

-- | Resets database by dropping all records.
resetDb ::
  DbConnection ->
  Bool ->
  IO ()
resetDb pool verbose = do
  runSqlPersistMPool clearAction pool
  when verbose $ putStrLn "Database successfully reset."
  where
    clearAction :: SqlPersistM ()
    clearAction = do
      deleteWhere ([] :: [Filter FileRecord])

-- | Does database migrations.
migrateDb ::
  DbConnection ->
  Bool ->
  IO ()
migrateDb pool verbose = do
  result <- runNoLoggingT $ runSqlPool (runMigrationQuiet migrateAll) pool

  when verbose $
    case result of
      [] -> putStrLn "Database migration not required."
      msgs ->
        putStrLn "Migration messages: "
          >> mapM_ (putStrLn . T.unpack) msgs

-- | Insert a single file record in the database.
insertFileRecord ::
  DbConnection ->
  Bool ->
  String ->
  Int64 ->
  String ->
  UTCTime ->
  IO (Key FileRecord)
insertFileRecord pool verbose path size digest mTime = do
  when verbose $ liftIO $ putStrLn $ "Adding " ++ path
  runNoLoggingT $ runSqlPool (insert $ FileRecord path size digest mTime) pool

-- | Inserts several file records in the database.
insertFileRecords ::
  DbConnection ->
  Bool ->
  [(String, Int64, String, UTCTime)] ->
  IO [Key FileRecord]
insertFileRecords pool verbose records = do
  when verbose $ liftIO $ putStrLn $ "Adding " ++ show (length records) ++ " records to database."
  runNoLoggingT $ runSqlPool (insertMany records') pool
  where
    records' = map (\(path, size, digest, mTime) -> FileRecord path size digest mTime) records

-- | Gets a file record from the database.
getFileRecord ::
  DbConnection ->
  String ->
  IO (Maybe (Int64, FileRecord))
getFileRecord pool path =
  runSqlPool (queryFileRecord path) pool
  where
    queryFileRecord :: String -> SqlPersistT IO (Maybe (Int64, FileRecord))
    queryFileRecord frp = do
      mEntity <- selectFirst [FileRecordPathName ==. frp] []
      case mEntity of
        Just (Entity fileId fileRecord) -> return $ Just (fromSqlKey fileId, fileRecord)
        Nothing -> return Nothing

-- | Updates a file record in the database.
updateFileRecord ::
  DbConnection ->
  Bool ->
  Int64 ->
  String ->
  Int64 ->
  String ->
  UTCTime ->
  IO ()
updateFileRecord pool verbose fileId path newSize newChecksum newModifiedTime = do
  when verbose $ putStrLn $ "Updated changed " ++ path ++ " file record in database."
  runSqlPool (updateFile (toSqlKey fileId) newSize newChecksum newModifiedTime) pool
  where
    updateFile :: Key FileRecord -> Int64 -> String -> UTCTime -> SqlPersistT IO ()
    updateFile fId size checksum modifiedTime = do
      update fId [FileRecordSize =. size, FileRecordMd5Checksum =. checksum, FileRecordModifiedAt =. modifiedTime]
