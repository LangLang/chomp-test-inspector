module Handler.StorageHandler (reloadFiles, loadFile) where

-- Application modules
import FileStore
import WebsocketApp (Clients)
import qualified STM.FileStore as STM (FileStore)

reloadFiles :: Clients -> STM.FileStore -> [FileInfo] -> IO ()
reloadFiles clients fileStore fileInfos  = return ()

loadFile :: Clients -> STM.FileStore -> FileInfo -> IO ()
loadFile clients fileStore fileInfo = return ()

