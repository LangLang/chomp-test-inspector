module Message (Message(..), Notification(..), StorageEvent(..), Patch(..)) where

{- 
  TODO: Possibly split Message into ServerMessage and ClientMessage or perhaps StorageMessage and
        EditorMessage. 
-}

-- Standard modules
import Data.Text

-- Application modules
import FileStore

data Message = Acknowledge
             | Notify Notification
             | ReloadFiles StorageEvent [FileInfo]
             | LoadFile StorageEvent FileInfo
             | PatchFile FilePath Patch
             | ParseError String
  deriving (Show, Read)
  
data Notification = Info String
                  | ClientDisconnected String
  deriving (Show, Read)
  
data StorageEvent = Connected
                  | ModifiedFile
                  | ModifiedDirectory
                  | MovedOutFile
                  | MovedInFile
                  | RenamedFile
                  | MovedOutDirectory
                  | MovedInDirectory
                  | RenamedDirectory
                  | MovedRootDirectory
                  | CreatedFile
                  | CreatedDirectory
                  | DeletedFile
                  | DeletedDirectory
                  | DeletedRootDirectory
                  | UnmountedRootDirectory
                  | Error
  deriving (Show, Read)

data Patch = D Text
  deriving (Show, Read)
