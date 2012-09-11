-- This module is responsible for keeping track of files being watched by the application
-- and caching data where convenient
module FileStore (
  module STM.FileStore,
  FileInfo(..), 
  FileContents, 
  FileStore,
  FileStoreEntry,
  FileCacheEntry,
  applyOperationalTransform
) where

-- Standard modules
import Data.Text (Text)

-- Supporting modules
-- https://github.com/timjb/haskell-operational-transformation
import qualified Control.OperationalTransformation.Text as OT
import qualified Control.OperationalTransformation.Server as OT

-- Application modules
import STM.FileStore hiding (FileStore, FileStoreEntry, FileCacheEntry)
import qualified STM.FileStore as STM (FileStore, FileStoreEntry, FileCacheEntry(..))

data FileInfo = FileInfo { 
    revision :: OT.Revision,
    operations :: [OT.TextOperation]
  }
  deriving (Show, Read)

type FileContents   = Text
type FileStore      = STM.FileStore FileInfo FileContents
type FileStoreEntry = STM.FileStoreEntry FileInfo FileContents
type FileCacheEntry = STM.FileCacheEntry FileInfo FileContents
type ServerState    = OT.ServerState FileContents OT.TextOperation  

otServerState :: FileCacheEntry -> ServerState
otServerState c = OT.ServerState (revision $ cacheEntryInfo c) (cacheEntryContents c) (operations $ cacheEntryInfo c)

-- Apply a text operation to a file in the file store
applyOperationalTransform :: FileCacheEntry -> (OT.Revision, [OT.Action]) -> Either String ([OT.Action], FileCacheEntry)
applyOperationalTransform cacheEntry (rev, actions) =
  let otResult = OT.applyOperation (otServerState cacheEntry) rev (OT.TextOperation actions) in
  case otResult of
    Left errorMessage -> Left errorMessage
    Right ((OT.TextOperation actions'), OT.ServerState revision' doc' ops') ->
      Right $ (actions', STM.FileCacheEntry (FileInfo { revision = revision', operations = ops' }) doc')
      
