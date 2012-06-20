module Message (Message(..), ServerMessage(..), Notification(..), StorageEvent(..), Patch(..)) where

{- 
  TODO: Possibly split Message into ServerMessage and ClientMessage or perhaps StorageMessage and
        EditorMessage. 
-}

-- Standard modules
import Data.Text

-- Application modules
import FileStore

-- Messages sent between clients and the server (and possibly between clients as well)
data Message = Acknowledge
             | Notify Notification
             | ReloadWatchPath
             | ReloadFiles StorageEvent [FileInfo]
             | LoadFile StorageEvent FileInfo
             | UnloadFile StorageEvent FileInfo
             | PatchFile FilePath Patch
             | ParseError String
  deriving (Show, Read)

-- Server generated messages (to be processed locally)
data ServerMessage = ServerReloadWatchPath
                   | ServerReloadFiles StorageEvent [FileInfo]
                   | ServerLoadFile StorageEvent FileInfo
                   | ServerUnloadFile StorageEvent FileInfo
                   | ServerModifiedFile
  deriving (Show, Read)

-- Notifications can be attached to certain messages
data Notification = Info String
                  | ClientDisconnected String
  deriving (Show, Read)

-- Certain messages carry storage events
data StorageEvent = Connected
                  | ModifiedFile
                  | ModifiedDirectory
                  | MovedOutFile
                  | MovedInFile
                  | RenamedFile
                  | MovedOutDirectory
                  | MovedInDirectory
                  | RenamedDirectory
                  | MovedOutRootDirectory
                  | RestoredRootDirectory
                  | CreatedFile
                  | CreatedDirectory
                  | DeletedFile
                  | DeletedDirectory
                  | DeletedRootDirectory
                  | UnmountedRootDirectory
                  | Error
  deriving (Show, Read)

-- Patch information for messages that carry text diffs
data Patch = D Text
  deriving (Show, Read)
