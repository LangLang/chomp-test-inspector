module Message (Message(..), Notification(..)) where

-- System modules
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
             | PatchFile FilePath Patch
             | ParseError String
  deriving (Show, Read)
