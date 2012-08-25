module Message (Message(..), ServerMessage(..), Notification(..), ProcessLog(..), StorageEvent(..), Patch(..), showSummary) where

{- 
  TODO: Possibly split Message into ServerMessage and ClientMessage or perhaps StorageMessage and
        EditorMessage. 
-}

-- Standard modules
import qualified Data.Text as T
import Data.Text (Text)
import System.Exit (ExitCode)

-- Supporting modules
-- https://github.com/timjb/haskell-operational-transformation
import qualified Control.OperationalTransformation.Text as OT
import qualified Control.OperationalTransformation.Server as OT (Revision)

-- Application modules
import FileStore

-- Messages sent between clients and the server (and possibly between clients as well)
data Message = Acknowledge
             | Notify Notification
             | ReloadFiles StorageEvent [FilePath]
             | LoadFile StorageEvent FilePath
             | UnloadFile StorageEvent FilePath
             | LoadFileContents FilePath OT.Revision FileContents
             | UnloadFileContents FilePath
             | OperationalTransform FilePath OT.Revision [OT.Action]
             | ParseError String
  deriving (Show, Read)

-- TODO
--type TimeStampedMessage = (UTCTime, Message)

-- Server generated messages (to be processed locally)
data ServerMessage = ServerNotify Notification
                   | ServerReloadFiles StorageEvent [FilePath]
                   | ServerLoadFile StorageEvent FilePath
                   | ServerUnloadFile StorageEvent FilePath
                   | ServerLoadFileContents FilePath FileContents
                   | ServerLoadModifications FilePath
                   | ServerOperationalTransform FilePath OT.Revision [OT.Action] 
                   | ServerExecuteAll
  deriving (Show, Read)

-- Notifications can be attached to certain messages
data Notification = Info String
                  | ClientDisconnected String
                  | ProcessMessage FilePath ProcessLog
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
  
-- Serialize a message summary (similar to `show`, but used for logging)
showSummary :: Message -> Text
showSummary (LoadFileContents f rev fc) = 
  T.pack "LoadFileContents "
  `T.append` (T.pack $ show f)
  `T.snoc` ' '
  `T.append` (T.pack $ show rev)
  `T.snoc` ' '
  `T.append` (showSummaryString fc)
showSummary (OperationalTransform path rev actions) =  
  T.pack "OperationalTransform " 
  `T.append` (T.pack $ show path) 
  `T.snoc` ' ' 
  `T.append` (T.pack $ show rev)
  `T.snoc` ' '
  `T.append` (showSummaryOTActions actions)
showSummary m = T.pack $ show m

showSummaryOTActions :: [OT.Action] -> Text
showSummaryOTActions actions = '['
  `T.cons` (if length actions > 5
    then (T.intercalate (T.pack ",") $ map showAction $ take 5 actions) `T.append` T.pack ",...]"  
    else (T.intercalate (T.pack ",") $ map showAction actions) `T.snoc` ']')
  where
    showAction (OT.Insert str) = (T.pack "Insert ") `T.append` showSummaryString str 
    showAction a               = T.pack $ show a

showSummaryString :: Text -> Text
showSummaryString str = '\"' 
  `T.cons` (if T.length str > 25
    then (replaceNewlinesTabs $ T.take 25 str) `T.append` (T.pack "...\"")   
    else (replaceNewlinesTabs str) `T.snoc` '\"')
  where
    replaceNewlinesTabs = T.unwords . T.split (`elem` ['\n', '\t'])
