-- This module is responsible for keeping track of files being watched by the application
-- and caching data where convenient
module FileStore (
  module STM.FileStore,
  FileInfo(..), 
  FileContents, 
  FileStore,
  FileStoreEntry,
  FileCacheEntry
) where

-- Standard modules
import Data.Text (Text)

-- Supporting modules
-- https://github.com/timjb/haskell-operational-transformation
import qualified Control.OperationalTransformation.Server as OT (Revision)

-- Application modules
import STM.FileStore hiding (FileStore, FileStoreEntry, FileCacheEntry) 
import qualified STM.FileStore as STM (FileStore, FileStoreEntry, FileCacheEntry)

data FileInfo = FileInfo { revision :: OT.Revision }
  deriving (Show, Read)
type FileContents = Text
type FileStore      = STM.FileStore FileInfo FileContents
type FileStoreEntry = STM.FileStoreEntry FileInfo FileContents
type FileCacheEntry = STM.FileCacheEntry FileInfo FileContents
