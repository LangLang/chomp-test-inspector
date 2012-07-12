module Handler.StorageHandler (handler) where

-- Application modules
import Message
import WebsocketApp (Clients)
import qualified STM.Clients
import qualified STM.FileStore as STM (FileStore)
import qualified STM.FileStore
import qualified STM.Messages as STM (ServerMessages)
import qualified Observer.WatchFile

handler :: STM.FileStore -> STM.ServerMessages -> Clients -> ServerMessage -> IO ()
handler fs sm c message = case message of

  -- Reload all files (or none)
  ServerReloadFiles event files ->
    STM.FileStore.reload fs files
    >> (STM.Clients.broadcastMessage c $ ReloadFiles event files)
    >> (Observer.WatchFile.loadFilesContents sm (STM.FileStore.rootPath fs) files)
    
  -- Load a file
  ServerLoadFile event file ->
    STM.FileStore.load fs file
    >> (STM.Clients.broadcastMessage c $ LoadFile event file)
    >> (Observer.WatchFile.loadFileContents sm (STM.FileStore.rootPath fs) file)

  -- Load a file's contents
  ServerLoadFileContents file fileContents ->
    STM.FileStore.loadContents fs file fileContents
    >> (STM.Clients.broadcastMessage c $ LoadFileContents file $ Just fileContents)
  
  -- Unload a file
  ServerUnloadFile event file -> 
    STM.FileStore.unload fs file
    >> (STM.Clients.broadcastMessage c $ UnloadFile event file)

  -- Unknown message
  _ -> undefined

--loadDiff :: Clients -> STM.FileStore -> FileInfo -> IO ()
--loadDiff clients fileStore file =
--  STM.Clients.broadcastMessage clients $ LoadDiff generateDiff
--  where
--    generateDiff :: IO Patch
--generateDiffPatch    generateDiff = 
