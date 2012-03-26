module Message (Message(..)) where

-- System modules
import Data.Text

-- Application modules
import FileStore

data Patch = D Text
  deriving (Show, Read)
data Message = Acknowledge | Notify String | ReloadFiles [FileInfo] | PatchFile FilePath Patch
  deriving (Show, Read)
