module Handler.ServerHandler (handler) where

-- System modules
import Data.Maybe (isJust, fromJust)
import qualified System.FilePath
import qualified System.IO
import Control.Monad (liftM)

-- Application modules
import Message
import qualified FileStore as FS
import FileStore (FileStore)
import WebsocketApp (Clients)
import Client (serverId)
import qualified STM.Clients
import qualified STM.Messages as STM (ServerMessages)
import qualified Observer.WatchFile
import qualified Observer.WatchExecutable

handler :: FileStore -> STM.ServerMessages -> Clients -> Maybe FilePath -> StampedMessage ServerMessage -> IO ()
handler fs sm c maybeExecPath (StampedMessage hostId time message) = case message of

  -- Notify clients of log messages
  ServerNotify notification -> 
    (STM.Clients.broadcastMessage c $ restampMessage $ Notify notification)

  -- Reload all files (or none)
  ServerReloadFiles event files ->
    FS.reloadIO fs files
    >> (STM.Clients.broadcastMessage c $ restampMessage $ ReloadFiles event files)
    >> (Observer.WatchFile.loadFilesContents sm (FS.rootPath fs) files)
    >> if isJust maybeExecPath 
      then Observer.WatchExecutable.runEach
        sm 
        (fromJust maybeExecPath) 
        (FS.rootPath fs) $ 
          filter ((== ".source") . System.FilePath.takeExtension) files
      else return ()
    
  -- Load a file
  ServerLoadFile event file -> do
    -- Use the file store to determine whether the file is merely being replaced (thus modified)
    alreadyLoaded <- liftM not $ FS.loadIO fs file
    if alreadyLoaded 
      then return ()
      else (STM.Clients.broadcastMessage c $ restampMessage $ LoadFile event file)
        >> (Observer.WatchFile.loadFileContents sm (FS.rootPath fs) file)
        >> if System.FilePath.takeExtension file == ".source" && isJust maybeExecPath 
          then Observer.WatchExecutable.run sm (fromJust maybeExecPath) (FS.rootPath fs) file
          else return ()

  -- Unload a file
  ServerUnloadFile event file -> 
    FS.unloadIO fs file
    >> (STM.Clients.broadcastMessage c $ restampMessage $ UnloadFile event file)
  
  -- Load a file's contents
  ServerLoadFileContents file fileContents ->
    FS.storeCacheIO fs file (FS.FileInfo { FS.contentsRevision = 0, FS.operations = [], FS.closedCounter = 0 }) fileContents 
    >> (STM.Clients.broadcastMessage c $ restampMessage $ LoadFileContents file 0 fileContents)
  
  -- Modified a file
  ServerLoadModifications file ->
    Observer.WatchFile.loadFileModifications sm fs file

  -- Broadcast operational transforms to the clients
  ServerOperationalTransform file rev actions opId ->
    -- Broadcast operations to clients
    (STM.Clients.broadcastMessage c $ restampMessage $ OperationalTransform file rev actions opId)
  
  -- Execute the tool on all files
  ServerExecuteAll -> do
    files <- FS.allFilesIO fs
    Observer.WatchExecutable.runEach
      sm
      (fromJust maybeExecPath) 
      (FS.rootPath fs) 
      (filter ((== ".source") . System.FilePath.takeExtension) files)

  -- Unknown message
  _ -> System.IO.hPutStrLn System.IO.stderr $ "Unhandled server message from "
    ++ (if hostId == serverId then "Server" else "Client #" ++ show hostId)
    ++ " (" ++ show time ++ "): "
    ++ show message
  
  where
    restampMessage = StampedMessage hostId time
