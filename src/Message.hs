module Message (Message(..)) where

-- Application modules
import FileStore

data Message = Acknowledge | Information String | ReloadFiles [FileInfo]
  deriving (Show, Read)
