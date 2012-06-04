module STM.FileStore (FileStore, rootPath, newIO, contents, clear, reload) where

-- Standard modules
import Control.Monad (liftM)
import Control.Concurrent.STM (atomically)
import Data.String.Utils (endswith)
import Data.STM.TList (TList)
import qualified Data.STM.TList as TList
import qualified System.Directory as Dir

-- Application modules
import FileStore

data FileStore = FileStore {
    rootPath :: FilePath,
    files :: TList FileInfo
  }

-- Create a new file store in a single atomic operation 
newIO :: FilePath -> IO FileStore
newIO path = do
  emptyFiles <- TList.emptyIO 
  return $ FileStore { rootPath = path, files = emptyFiles }
  
-- Get the contents of the file store as a single atomic operation 
contents :: FileStore -> IO [FileInfo]
contents fs = atomically $ TList.toList $ files fs

-- Clear the file store using multiple operations 
clear :: FileStore -> IO ()
clear fs = do
  done <- atomically $ do
    isEmpty <- TList.null fsFiles 
    if not isEmpty
      then TList.drop 1 fsFiles >> return False
      else return True
  if done
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
  atomically $ TList.appendList fsFiles directoryContents
  >>= atomically . TList.toList
  where
    isDots f = (endswith "/." f) || (endswith "/.." f) || (f == "..") || (f == ".")
    fsFiles = files fs
    fsRootPath = rootPath fs