-- This module is responsible for keeping track of files being watched by the application
-- and caching data where convenient
module FileStore (
  module STM.FileStore,
  FileInfo(..), 
  FileContents, 
  FileStore,
  FileStoreEntry,
  FileCacheEntry,
  opsRevision,
  mergeAtContentsRevision,
  updateFileContentsIO,
  readPendingFileContents
) where

-- Standard modules
import Data.Text (Text)

-- Supporting modules
-- https://github.com/timjb/haskell-operational-transformation
import qualified Control.OperationalTransformation.Text as OT
import qualified OTServer as OT

-- Application modules
import STM.FileStore hiding (FileStore, FileStoreEntry, FileCacheEntry)
import qualified STM.FileStore as STM (FileStore, FileStoreEntry, FileCacheEntry(..))

data FileInfo = FileInfo {
    -- TODO (Performance): Possibly each of these should be TVar's to make for smaller STM
    --                     transactions... It makes sense to access the dirty bit separately from 
    --                     operations as these have separate uses
     
    contentsRevision :: OT.Revision,  -- Revision number of the (persisted) file contents
                                      -- (this may be < than the number of operations while waiting
                                      -- for the system to persist changes to secondary storage)
    operations :: [OT.TextOperation], -- List of operations applied to the file (and yet to be applied) since being loaded
    closedCounter :: Int              -- Keeps track of the number of times the file handle was closed by 
                                      -- the file store in order to prevent reloading it each time  
  }
  deriving (Show, Read)

type FileContents   = Text
type FileStore      = STM.FileStore FileInfo FileContents
type FileStoreEntry = STM.FileStoreEntry FileInfo FileContents
type FileCacheEntry = STM.FileCacheEntry FileInfo FileContents

-- Accessor function to get the operations revision of a file info
opsRevision :: FileInfo -> OT.Revision
opsRevision fi = fromIntegral $ length (operations fi)  

-- Merge an operation at the contents revision of the cached file
mergeAtContentsRevision :: FileCacheEntry -> OT.TextOperation -> Either String FileCacheEntry
mergeAtContentsRevision cacheEntry op = do 
  let 
    fi = cacheEntryInfo cacheEntry
    fc = cacheEntryContents cacheEntry
    ops = operations fi
    rev = contentsRevision fi
  ops' <- OT.mergeAtRevision ops rev op
  return $ STM.FileCacheEntry (fi { operations = ops' }) fc

-- Apply remaining operations to the file store 
updateFileContentsIO :: FileStore -> FilePath -> FileCacheEntry -> IO (Either String FileCacheEntry)
updateFileContentsIO fs f cacheEntry =
  let
    fi = cacheEntryInfo cacheEntry
    fc = cacheEntryContents cacheEntry
    ops = operations fi 
    rev = contentsRevision fi
  in case OT.applyAtRevision ops rev fc of
    Left err -> return $ Left $ "Operational transform failed: " ++ err
    Right fc' ->
      let fi' = (cacheEntryInfo cacheEntry) { contentsRevision = fromIntegral $ length ops }
          cacheEntry'' = STM.FileCacheEntry fi' fc'
      in do
        found <- storeCacheEntryIO fs f cacheEntry''
        return $ if found
          then Right cacheEntry'' 
          else Left $ "Could not find file store entry for the file '" ++ f ++ "'"

-- Reads the cached file contents with all pending operations already applied 
readPendingFileContents :: FileCacheEntry -> Either String FileContents
readPendingFileContents cacheEntry =
  let fi = cacheEntryInfo cacheEntry
      fc = cacheEntryContents cacheEntry
      ops = operations fi
      rev = contentsRevision fi 
  in OT.applyAtRevision ops rev fc 
