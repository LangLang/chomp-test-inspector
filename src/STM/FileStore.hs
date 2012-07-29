module STM.FileStore (FileStore, rootPath, newIO, allFiles, readFileContents, clear, reload, load, loadContents, unload) where

-- The file store is a cache that reflects the contents of a location on some storage device.
-- Its only responsibility is storage and synchronicity (atomicity) of storage access - the file
-- store itself does not get involved in physical disk access or messaging tasks.   

-- Standard modules
import Control.Monad (liftM, (<=<), zipWithM, filterM)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar as TVar
import Data.Text (Text)
import Data.Maybe (isNothing)
import qualified Data.STM.TList as TList
import qualified Data.STM.TCursor as TCursor
import Data.STM.TCursor (TCursor)
import Prelude hiding (readFile)

-- Application modules
import FileStore

data FileStoreEntry = FileStoreEntry { 
    fileInfo :: TVar FileInfo,
    fileContents :: TVar (Maybe Text)
  }

data FileStore = FileStore {
    rootPath :: FilePath,
    files :: TCursor FileStoreEntry
  }

-- Create a new file store in a single atomic operation 
newIO :: FilePath -> IO FileStore
newIO path = do
  emptyFiles <- atomically $ TList.empty >>= TVar.newTVar
  return $ FileStore { rootPath = path, files = emptyFiles }
  
-- Get the contents of the file store as a single atomic operation 
allFiles :: FileStore -> IO [FileInfo]
allFiles fs = do  
  fileStoreEntries <- atomically $ (TList.toList <=< TVar.readTVar) $ files fs
  mapM (TVar.readTVarIO . fileInfo) fileStoreEntries

-- Read the contents 
readFileContents :: FileStore -> FilePath -> IO (Maybe Text)
readFileContents fs f = do 
  maybeEntry <- (readFileStoreEntry fs f)
  case maybeEntry of
    Nothing -> return Nothing
    Just entry -> readTVarIO $ fileContents entry

-- Clear the file store using multiple operations
clear :: FileStore -> IO ()
clear fs = do
  xs <- atomically $ TCursor.tryReadTCursor $ files fs
  if isNothing xs
    then return ()
    else clear fs

-- Reload the file store
reload :: FileStore -> [FileInfo] -> IO ()
reload fs filesToLoad = do
  entries <- zipWithM createFileStoreEntry filesToLoad $ repeat Nothing
  _ <- clear fs
  _ <- atomically $ do
    filesList <- readTVar $ files fs
    flip TList.appendList entries filesList
  return ()
    
-- Load a single file into the file store
load :: FileStore -> FileInfo -> IO ()
load fs f = do
  newEntry <- createFileStoreEntry f Nothing
  _ <- atomically $ do
    oldList <- readTVar $ files fs
    -- Note: Using TList.cons would probably be faster than TList.append because we're not keeping a tlist
    --       end-point. However, append keeps the file listing order on the clients more or less
    --       consistent
    writeEnd <- TList.end oldList 
    TList.append writeEnd newEntry
  return ()

-- Load file contents into the store
loadContents :: FileStore -> FileInfo -> Text -> IO Bool
loadContents fs f contents = do
  maybeEntry <- readFileStoreEntry fs f
  case maybeEntry of
    Nothing -> return False
    Just entry ->
      (atomically $ writeTVar (fileContents entry) $ Just contents)
      >> return True

-- Remove the file from the file store
unload :: FileStore -> FileInfo -> IO ()
unload fs f =
  atomically $ do
    oldTList <- readTVar $ files fs
    oldList <- TList.toList oldTList
    newList <- filterM notIsFile oldList
    newTLists <- TList.fromList newList
    writeTVar (files fs) (fst newTLists)
  where
    notIsFile :: FileStoreEntry -> STM Bool
    notIsFile entry = (liftM (/= f)) $ readTVar $ fileInfo entry

-- Generate a difference patch for a file in the store that was modified on disk
--generateDiffPatch :: IO Patch
--generateDiffPatch = return  

-- Helper to generate a new file store entry (used internally)
createFileStoreEntry :: FileInfo -> Maybe Text -> IO FileStoreEntry
createFileStoreEntry info contents = do
  tFileInfo <- newTVarIO info
  tFileContents <- newTVarIO contents
  return $ FileStoreEntry { fileInfo = tFileInfo, fileContents = tFileContents }

-- Read a specific file entry from the file store (used internally)
readFileStoreEntry :: FileStore -> FilePath -> IO (Maybe FileStoreEntry)
readFileStoreEntry fs f = do
  searchList <- 
    (readTVarIO $ files fs) 
    >>= (atomically . TList.toList)
  find searchList 
  where
    -- Try to find the file in the list of file store entry (atomically reading each file's entry)
    find :: [FileStoreEntry] -> IO (Maybe FileStoreEntry)
    find [] = return Nothing
    find (x:xs) = do 
      result <- matchFile x
      if result 
        then return $ Just x
        else find xs
    
    matchFile :: FileStoreEntry -> IO Bool
    matchFile entry = 
      (readTVarIO $ fileInfo entry)
      >>= return . (f ==)
