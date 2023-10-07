module HugoIncr (incrementTarget) where

import Conduit
import Control.Monad (unless, void, when)
import Crypto.Hash (Digest, MD5)
import Crypto.Hash.Conduit (sinkHash)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)
import qualified Db as DB
import Models
import System.Directory

chunkSize :: Int
chunkSize = 1000

-- | Processes a single file.
processFile ::
  DB.DbConnection ->
  String ->
  Bool ->
  FilePath ->
  IO [(String, Int64, String, UTCTime)]
processFile pool prefix verbose filePath = do
  let path = fromMaybe filePath (stripPrefix prefix filePath)
  fileSize <- getSizeOfFile filePath
  modificationTime <- getModificationTimeOfFile filePath
  md5Digest <-
    runConduitRes $
      sourceFile filePath
        .| (sinkHash :: ConduitT BS.ByteString o (ResourceT IO) (Digest MD5))
  let md5Checksum = show md5Digest

  mFileRecord <- DB.getFileRecord pool path
  case mFileRecord of
    Just (fileId, fileRecord) -> do
      if fileSize == fileRecordSize fileRecord
        && md5Checksum == fileRecordMd5Checksum fileRecord
        then unless
          (modificationTime == fileRecordModifiedAt fileRecord)
          $ do
            when verbose $ putStrLn $ "Touched " ++ filePath
            updateFileModifiedTime
              filePath
              (fileRecordModifiedAt fileRecord)
        else
          DB.updateFileRecord
            pool
            verbose
            fileId
            path
            fileSize
            md5Checksum
            modificationTime

      return []
    Nothing -> do
      return [(path, fileSize, md5Checksum, modificationTime)]
  where
    -- Function to get the size of a file
    getSizeOfFile :: FilePath -> IO Int64
    getSizeOfFile fp = do
      size <- getFileSize fp
      return (fromIntegral size)

    -- Function to get the modification time of a file
    getModificationTimeOfFile :: FilePath -> IO UTCTime
    getModificationTimeOfFile fp = do
      getModificationTime fp

    -- Update the modified time of a file at the given path
    updateFileModifiedTime :: FilePath -> UTCTime -> IO ()
    updateFileModifiedTime fp aTime = do
      setModificationTime fp aTime

-- | Custom conduit to process individual files and yield chunks of file records.
processFileConduit ::
  MonadIO m =>
  DB.DbConnection ->
  String ->
  Bool ->
  ConduitT FilePath [(String, Int64, String, UTCTime)] m ()
processFileConduit pool prefix verbose = awaitForever $ \filePath -> do
  frs <- liftIO $ processFile pool prefix verbose filePath
  yield frs

-- | Process a single FileRecord.
processRecord ::
  DB.DbConnection ->
  String ->
  Bool ->
  (Int64, FileRecord) ->
  ResourceT IO ()
processRecord pool prefix verbose (key, fileRecord) = do
  let path = prefix ++ fileRecordPathName fileRecord
  pathExists <- liftIO $ doesPathExist path
  unless pathExists $ liftIO $ DB.deleteFileRecordById pool verbose key path

-- | Increments files under specified Hugo public.
incrementTarget ::
  DB.DbConnection ->
  Bool ->
  String ->
  String ->
  String ->
  IO ()
incrementTarget pool verbose path ps target = do
  let rootPath = path ++ ps ++ target
  let prefixPath = path ++ ps

  -- Process paths and add to or update in database.
  runConduitRes $
    sourceDirectoryDeep False rootPath
      .| processFileConduit pool prefixPath verbose
      .| chunksOfCE chunkSize
      .| mapM_C (liftIO . void . DB.insertFileRecords pool verbose)

  -- Clear records that are (no longer) on filesystem
  runConduitRes $
    DB.retrieveAllFileRecords pool
      .| mapM_C (processRecord pool prefixPath verbose)
