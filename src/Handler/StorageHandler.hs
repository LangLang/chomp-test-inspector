module Handler.StorageHandler (reloadWatchPath, loadFile, unloadFile) where

-- Application modules
import Message
import WebsocketApp (Clients)
import FileStore
import qualified STM.Clients
import qualified STM.FileStore as STM (FileStore)
import qualified STM.FileStore

reloadWatchPath :: Clients -> STM.FileStore -> [FileInfo] -> IO ()
reloadWatchPath clients fileStore files = do
  errorOrFiles <- STM.FileStore.reload fileStore files
  STM.Clients.broadcastMessage clients $ ReloadFiles RestoredRootDirectory files

loadFile :: Clients -> STM.FileStore -> StorageEvent -> FileInfo -> IO ()
loadFile clients fileStore event file =
  STM.FileStore.load fileStore file
  >> do
    files <- STM.FileStore.allFiles fileStore
    putStrLn $ "Load file " ++ (show file) ++ "... new filestore:"
    putStrLn $ show files
  >> (STM.Clients.broadcastMessage clients $ LoadFile event file)
  
unloadFile :: Clients -> STM.FileStore -> StorageEvent -> FileInfo -> IO ()
unloadFile clients fileStore event file =
  STM.FileStore.unload fileStore file
  >> do
    files <- STM.FileStore.allFiles fileStore
    putStrLn $ "Unload file " ++ (show file) ++ "... new filestore:"
    putStrLn $ show files
  >> (STM.Clients.broadcastMessage clients $ UnloadFile event file)

--loadDiff :: Clients -> STM.FileStore -> FileInfo -> IO ()
--loadDiff clients fileStore file =
--  STM.Clients.broadcastMessage clients $ LoadDiff generateDiff
--  where
--    generateDiff :: IO Patch
--generateDiffPatch    generateDiff = 
