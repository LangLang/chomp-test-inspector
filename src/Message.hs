module Message (Message(..), Notification(..)) where

-- Standard modules
import Data.Text

-- Application modules
import FileStore

data Patch = D Text
  deriving (Show, Read)
data Notification = Info String
                  | ClientDisconnected String
  deriving (Show, Read)
data Message = Acknowledge
             | Notify Notification
             | ReloadFiles [FileInfo]
             | LoadFile FileInfo
             | PatchFile FilePath Patch
             | ParseError String
  deriving (Show, Read)
