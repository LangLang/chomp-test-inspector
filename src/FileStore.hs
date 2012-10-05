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
  applyOperationToCache -- remove?
) where

-- Standard modules
import Data.Text (Text)
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Concurrent.STM (STM)

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
    --closedCounter :: STM.TVar Int
    closedCounter :: Int              -- Keeps track of the number of times the file handle was closed by 
                                      -- the file store in order to prevent reloading it each time  
  }
  deriving (Show, Read)

type FileContents   = Text
type FileStore      = STM.FileStore FileInfo FileContents
type FileStoreEntry = STM.FileStoreEntry FileInfo FileContents
type FileCacheEntry = STM.FileCacheEntry FileInfo FileContents
--type ServerState    = OT.ServerState FileContents OT.TextOperation

--otServerState :: FileCacheEntry -> ServerState
--otServerState c = OT.ServerState (revision $ cacheEntryInfo c) (cacheEntryContents c) (operations $ cacheEntryInfo c)

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

{-
mergeAtContentsRevisionStoreIO :: FileStore -> FilePath -> FileCacheEntry -> OT.TextOperation -> IO (Either String FileCacheEntry)
mergeAtContentsRevisionStoreIO fs f cacheEntry op =
  case mergeAtContentsRevision cacheEntry op of
    Left err -> return $ Left $ "Operational transform failed: " ++ err
    Right cacheEntry' -> do
      result <- storeCacheEntryIO fs f cacheEntry'
      return $ if result
        then Right cacheEntry'
        else Left $ "Could not store result of operational transform in the file store"
  -}
    
{-
-- Apply remaining operations to the file store and mark the entry as not dirty 
updateFileContentSTM :: FileStore -> FilePath -> FileCacheEntry -> Either String (STM ())
updateFileContentSTM fs f cacheEntry =
  let
    fi = cacheEntryInfo cacheEntry
    fc = cacheEntryContents cacheEntry
    ops = operations fi 
    rev = contentsRevision fi
  in case OT.applyAtRevision ops rev fc of
    Left err -> Left $ "Operational transform failed: " ++ err
    Right cacheEntry' -> storeCacheEntrySTM fs f cacheEntry'
-}

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

{-
-- Apply a text operation to a file in the file store
applyOperationalTransform :: FileCacheEntry -> (OT.Revision, [OT.Action]) -> Either String ([OT.Action], FileCacheEntry)
applyOperationalTransform cacheEntry (rev, actions) =
  let otResult = OT.applyOperation (otServerState cacheEntry) rev (OT.TextOperation actions) in
  case otResult of
    Left errorMessage -> Left errorMessage
    Right ((OT.TextOperation actions'), OT.ServerState revision' doc' ops') ->
      Right $ (actions', STM.FileCacheEntry (FileInfo { revision = revision', operations = ops' }) doc')
-}

applyOperationToCache :: FileCacheEntry -> (OT.Revision, OT.TextOperation) -> Either String FileCacheEntry
applyOperationToCache cacheEntry (rev,op) =
  let
    fileInfo = cacheEntryInfo cacheEntry
    precedingOps = operations fileInfo  
    otResult = OT.mergeAtRevision precedingOps rev op in
  do 
    ops' <- otResult
    return $ (STM.FileCacheEntry (fileInfo { operations = ops' }) (cacheEntryContents cacheEntry))

-- The write operation is application specific because it needs to test the dirty flag to determine whether it may proceed
-- and applies the operational transform only once the file been properly locked (and the dirty flag was determined to be false)
-- This function will also wait a second after locking the file, just to make sure that the WatchDirectory 
-- module has had time to update the dirty bit if necessary  
{-
writeFileContentsIO :: FileStore -> FilePath -> [OT.TextOperation] -> IO ()
writeFileContentsIO fs f ops = runMaybeT $ do
  entry <- liftM $ readFileStoreEntryIO fs f
  return ()
  { -case maybeEntry of
    Nothing -> return Nothing
    Just fileEntry -> do
      maybeCacheEntry <- fileEntryCacheIO fileEntry
      case maybeCacheEntry of
        Nothing -> return Nothing
        Just cacheEntry ->
          return $ Just $ cacheEntryContents cacheEntry
-}