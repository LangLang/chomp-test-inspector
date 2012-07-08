module Handler.StorageHandler (reloadFiles, loadFile, loadFileContents, unloadFile) where

-- Standard modules
import Data.Text

-- Application modules
import Message
import WebsocketApp (Clients)
import FileStore
import qualified STM.Clients
import qualified STM.FileStore as STM (FileStore)
import qualified STM.FileStore
  
reloadFiles :: Clients -> STM.FileStore -> StorageEvent -> [FileInfo] -> IO ()
reloadFiles clients fileStore event files =
  STM.FileStore.reload fileStore files
  >> (STM.Clients.broadcastMessage clients $ ReloadFiles event files)

loadFile :: Clients -> STM.FileStore -> StorageEvent -> FileInfo -> IO ()
loadFile clients fileStore event file =
  STM.FileStore.load fileStore file
  >> (STM.Clients.broadcastMessage clients $ LoadFile event file)
  
loadFileContents :: Clients -> STM.FileStore -> FileInfo -> Text -> IO ()
loadFileContents clients fileStore filePath fileContents =
  STM.FileStore.loadContents fileStore filePath fileContents
  >> (STM.Clients.broadcastMessage clients $ LoadFileContents filePath $ Just fileContents)
  
unloadFile :: Clients -> STM.FileStore -> StorageEvent -> FileInfo -> IO ()
unloadFile clients fileStore event file =
  STM.FileStore.unload fileStore file
  >> (STM.Clients.broadcastMessage clients $ UnloadFile event file)

--loadDiff :: Clients -> STM.FileStore -> FileInfo -> IO ()
--loadDiff clients fileStore file =
--  STM.Clients.broadcastMessage clients $ LoadDiff generateDiff
--  where
--    generateDiff :: IO Patch
--generateDiffPatch    generateDiff = 
