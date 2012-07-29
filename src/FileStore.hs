-- This module is responsible for keeping track of files being watched by the application
-- and caching data where convenient
module FileStore (FileInfo, FileContents(..)) where

-- Standard modules
import Data.Text (Text)

-- Supporting modules
-- https://github.com/timjb/haskell-operational-transformation
import qualified Control.OperationalTransformation.Server as OT (Revision)

--type FileStore = [FilePath]
type FileInfo = FilePath
data FileContents = FileContents Text OT.Revision
  deriving (Show, Read)