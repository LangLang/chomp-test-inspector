module STM.FileStore (
  FileStore,
  FileCacheEntry(..),
  FileStoreEntry,
  rootPath,
  newIO,
  allFilesIO,
  readFileStoreEntryIO,
  readFileCacheEntryIO,
  readFileContentsIO,
  clearIO,
  reloadIO,
  loadIO,
  loadCacheIO,
  unloadIO,
  fileEntryPathT,
  fileEntryCacheT,
  fileEntryPathIO,
  fileEntryCacheIO
) where

-- The file store is a cache that reflects the contents of a location on some storage device.
-- Its only responsibility is storage and synchronicity (atomicity) of storage access - the file
-- store itself does not get involved in physical disk access or messaging tasks.   

-- Standard modules
import Control.Monad (liftM, (<=<), filterM)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar as TVar
import Data.Maybe (isNothing)
import qualified Data.STM.TList as TList
import qualified Data.STM.TCursor as TCursor
import Data.STM.TCursor (TCursor)
import Prelude hiding (readFile)

data FileCacheEntry fileInfoType fileContentsType = FileCacheEntry {
    cacheEntryInfo :: fileInfoType,
    cacheEntryContents :: fileContentsType
  }

data FileStoreEntry fileInfoType fileContentsType = FileStoreEntry {
    fileEntryPath :: TVar FilePath,
    fileEntryCache :: TVar (Maybe (FileCacheEntry fileInfoType fileContentsType))
  }
  
fileEntryPathT :: (FileStoreEntry fiType fcType) -> STM FilePath
fileEntryPathT = readTVar . fileEntryPath

fileEntryPathIO :: (FileStoreEntry fiType fcType) -> IO FilePath
fileEntryPathIO = readTVarIO . fileEntryPath

fileEntryCacheT :: (FileStoreEntry fiType fcType) -> STM (Maybe (FileCacheEntry fiType fcType))
fileEntryCacheT = readTVar . fileEntryCache

fileEntryCacheIO :: (FileStoreEntry fiType fcType) -> IO (Maybe (FileCacheEntry fiType fcType))
fileEntryCacheIO = readTVarIO . fileEntryCache

data FileStore fileInfoType fileContentsType = FileStore {
    rootPath :: FilePath,
    files :: TCursor (FileStoreEntry fileInfoType fileContentsType)
  }

-- Create a new file store in a single atomic operation 
newIO :: FilePath -> IO (FileStore fiType fcType)
newIO path = do
  emptyFiles <- atomically $ TList.empty >>= TVar.newTVar
  return $ FileStore { rootPath = path, files = emptyFiles }
  
-- Get the contents of the file store as a single atomic operation 
allFilesIO :: (FileStore fiType fcType) -> IO [FilePath]
allFilesIO fs = do  
  fileStoreEntries <- atomically $ (TList.toList <=< TVar.readTVar) $ files fs
  mapM (TVar.readTVarIO . fileEntryPath) fileStoreEntries

-- Read the contents 
readFileContentsIO :: (FileStore fiType fcType) -> FilePath -> IO (Maybe fcType)
readFileContentsIO fs f = do 
  maybeEntry <- (readFileStoreEntryIO fs f)
  case maybeEntry of
    Nothing -> return Nothing
    Just fileEntry -> do
      maybeCacheEntry <- fileEntryCacheIO fileEntry
      case maybeCacheEntry of
        Nothing -> return Nothing
        Just cacheEntry ->
          return $ Just $ cacheEntryContents cacheEntry

{- For presentation: Why is this not possible?
readFileContentsIO :: (FileStore fiType fcType) -> FilePath -> IO (Maybe fcType)
readFileContentsIO fs f = do
  fileEntry <- (readFileStoreEntryIO fs f)
  cacheEntry <- fileEntryCacheIO fileEntry
  return $ fileCacheContents cacheEntry
--} 

-- Clear the file store using multiple operations
clearIO :: (FileStore fiType fcType) -> IO ()
clearIO fs = do
  xs <- atomically $ TCursor.tryReadTCursor $ files fs
  if isNothing xs
    then return ()
    else clearIO fs

-- Reload the file store
reloadIO :: (FileStore fiType fcType) -> [FilePath] -> IO ()
reloadIO fs filesToLoad = do
  entries <- mapM createFileStoreEntryIO filesToLoad
  _ <- clearIO fs
  _ <- atomically $ do
    filesList <- readTVar $ files fs
    flip TList.appendList entries filesList
  return ()
    
-- Load a single file into the file store
-- TODO: rename this to addFile
loadIO :: (FileStore fiType fcType) -> FilePath -> IO Bool
loadIO fs f = do
  existingEntry <- readFileStoreEntryIO fs f
  case existingEntry of
    Just _ -> return False
    Nothing -> do
      newEntry <- createFileStoreEntryIO f
      _ <- atomically $ do
        oldList <- readTVar $ files fs
        -- Note: Using TList.cons would probably be faster than TList.append because we're not keeping a tlist
        --       end-point. However, append keeps the file listing order on the clients more or less
        --       consistent
        writeEnd <- TList.end oldList 
        TList.append writeEnd newEntry
      return True

-- Load file cache entry into the store
loadCacheIO :: (FileStore fiType fcType) -> FilePath -> fiType -> fcType -> IO Bool
loadCacheIO fs f fi fc = do
  maybeEntry <- readFileStoreEntryIO fs f
  case maybeEntry of
    Nothing -> return False
    Just fileEntry ->
      (atomically $ writeTVar (fileEntryCache fileEntry) $ Just $ FileCacheEntry { cacheEntryInfo = fi, cacheEntryContents = fc})
      >> return True

-- Remove the file from the file store
unloadIO :: (FileStore fiType fcType) -> FilePath -> IO ()
unloadIO fs f =
  atomically $ do
    oldTList <- readTVar $ files fs
    oldList <- TList.toList oldTList
    newList <- filterM notIsFile oldList
    newTLists <- TList.fromList newList
    writeTVar (files fs) (fst newTLists)
  where
    notIsFile :: (FileStoreEntry fiType fcType) -> STM Bool
    notIsFile entry = (liftM (/= f)) $ readTVar $ fileEntryPath entry

-- Generate a difference patch for a file in the store that was modified on disk
--generateDiffPatch :: IO Patch
--generateDiffPatch = return  

-- Helper to generate a new file store entry (used internally)
createFileStoreEntryIO :: FilePath -> IO (FileStoreEntry fiType fcType)
createFileStoreEntryIO f = do
  tFilePath <- newTVarIO f
  tFileCache <- newTVarIO Nothing
  return $ FileStoreEntry { 
      fileEntryPath = tFilePath, 
      fileEntryCache = tFileCache 
    }

-- Read a specific file entry from the file store (used internally)
readFileStoreEntryIO :: (FileStore fiType fcType) -> FilePath -> IO (Maybe (FileStoreEntry fiType fcType))
readFileStoreEntryIO fs f = do
  searchList <- 
    (readTVarIO $ files fs) 
    >>= (atomically . TList.toList)
  find searchList 
  where
    -- Try to find the file in the list of file store entry (atomically reading each file's entry)
    find :: [FileStoreEntry fiType fcType] -> IO (Maybe (FileStoreEntry fiType fcType))
    find [] = return Nothing
    find (x:xs) = do 
      result <- matchFile x
      if result 
        then return $ Just x
        else find xs
    
    matchFile :: (FileStoreEntry fiType fcType) -> IO Bool
    matchFile entry = 
      (readTVarIO $ fileEntryPath entry)
      >>= return . (f ==)

-- Read a specific file entry from the file store (used internally)
readFileCacheEntryIO :: (FileStore fiType fcType) -> FilePath -> IO (Maybe (FileCacheEntry fiType fcType))
readFileCacheEntryIO fs f = do
  maybeFileEntry <- readFileStoreEntryIO fs f
  case maybeFileEntry of
    Nothing -> return Nothing
    Just fileEntry -> fileEntryCacheIO fileEntry 
