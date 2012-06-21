module STM.FileStore (FileStore, rootPath, newIO, contents, clear, reload, load, unload) where

-- Standard modules
import Control.Monad (liftM, (<=<))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar as TVar
import Data.Maybe (isNothing)
import Data.String.Utils (endswith)
import qualified Data.STM.TList as TList
import qualified Data.STM.TCursor as TCursor
import Data.STM.TCursor (TCursor)
import qualified System.Directory as Dir

-- Application modules
import FileStore

data FileStore = FileStore {
    rootPath :: FilePath,
    files :: TCursor FileInfo
  }

-- Create a new file store in a single atomic operation 
newIO :: FilePath -> IO FileStore
newIO path = do
  emptyFiles <- atomically $ TList.empty >>= TVar.newTVar
  return $ FileStore { rootPath = path, files = emptyFiles }
  
-- Get the contents of the file store as a single atomic operation 
contents :: FileStore -> IO [FileInfo]
contents fs = atomically $ (TList.toList <=< TVar.readTVar) $ files fs

-- Clear the file store using multiple operations 
clear :: FileStore -> IO ()
clear fs = do
  xs <- atomically $ TCursor.tryReadTCursor fsFiles
  if isNothing xs
    then return ()
    else clear fs
  where 
    fsFiles = files fs

-- Reload the file store
reload :: FileStore -> IO [FileInfo]
reload fs = do
  -- Get the initial contents of the directory being watched
  directoryContents <- (liftM $ filter $ not . isDots) $ Dir.getDirectoryContents fsRootPath
  _ <- clear fs
  _ <- atomically $ (flip TList.appendList directoryContents <=< readTVar) fsFiles
  atomically $ (TList.toList <=< readTVar) fsFiles 
  where
    isDots f = (endswith "/." f) || (endswith "/.." f) || (f == "..") || (f == ".")
    fsFiles = files fs
    fsRootPath = rootPath fs
    
-- Load a single file into the file store
load :: FileStore -> FileInfo -> IO ()
load fs f = 
  atomically $ (flip TList.append f <=< readTVar) fsFiles 
  >> return ()
  where
    fsFiles = files fs

-- Remove the file from the file store
unload :: FileStore -> FileInfo -> IO ()
unload fs f = 
  atomically $ (flip TList.append f <=< readTVar) fsFiles 
  >> return ()
  where
    fsFiles = files fs

-- Generate a difference patch for a file in the store that was modified on disk
--generateDiffPatch :: IO Patch
--generateDiffPatch = return  
