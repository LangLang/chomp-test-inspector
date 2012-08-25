-- This module is responsible for keeping track of files being watched by the application
-- and caching data where convenient
module FileStore (
  module STM.FileStore,
  FileInfo(..), 
  FileContents, 
  FileStore,
  FileStoreEntry,
  FileCacheEntry,
  otServerState
) where

-- Standard modules
import Data.Text (Text)

-- Supporting modules
-- https://github.com/timjb/haskell-operational-transformation
import qualified Control.OperationalTransformation.Text as OT (TextOperation)
import qualified Control.OperationalTransformation.Server as OT (ServerState(..), Revision)

-- Application modules
import STM.FileStore hiding (FileStore, FileStoreEntry, FileCacheEntry) 
import qualified STM.FileStore as STM (FileStore, FileStoreEntry, FileCacheEntry)

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