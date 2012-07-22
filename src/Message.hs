module Message (Message(..), ServerMessage(..), Notification(..), ProcessLog(..), StorageEvent(..), Patch(..)) where

{- 
  TODO: Possibly split Message into ServerMessage and ClientMessage or perhaps StorageMessage and
        EditorMessage. 
-}

-- Standard modules
import Data.Text
import System.Exit (ExitCode)

-- Application modules
import FileStore

-- Messages sent between clients and the server (and possibly between clients as well)
data Message = Acknowledge
             | Notify Notification
             | ReloadFiles StorageEvent [FileInfo]
             | LoadFile StorageEvent FileInfo
             | LoadFileContents FileInfo (Maybe Text)
             | UnloadFile StorageEvent FileInfo
             | PatchFile FilePath Patch
             | ParseError String
  deriving (Show, Read)

-- TODO
--type TimeStampedMessage = (UTCTime, Message)

-- Server generated messages (to be processed locally)
data ServerMessage = ServerReloadFiles StorageEvent [FileInfo]
                   | ServerLoadFile StorageEvent FileInfo
                   | ServerLoadFileContents FileInfo Text
                   | ServerUnloadFile StorageEvent FileInfo
                   | ServerModifiedFile
                   | ServerExecuteAll
  deriving (Show, Read)

-- Notifications can be attached to certain messages
data Notification = Info String
                  | ClientDisconnected String
                  | ProcessOutput FileInfo ProcessLog
  deriving (Show, Read)
  
-- The log messages from a running process may be sent as a notification
data ProcessLog = LogStart 
                | LogInfo String
                | LogError String
                | LogEnd ExitCode -- TODO: add exit code?
  deriving (Show, Read)

-- Certain messages carry storage events
data StorageEvent = WatchInstalled
                  | Connected
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
