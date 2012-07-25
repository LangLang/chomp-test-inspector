module Observer.WatchFile (loadFileContents, loadFilesContents, loadFileModifications) where

-- Standard modules
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Control.Concurrent
import qualified System.FilePath

-- Supporting modules
import qualified Control.OperationalTransformation.Text as OT

-- Application modules
import Message
import qualified STM.Messages as STM (ServerMessages)
import qualified STM.Messages
import qualified STM.FileStore as STM (FileStore)
import qualified STM.FileStore

loadFileContents :: STM.ServerMessages -> FilePath -> FilePath -> IO ()
loadFileContents messages rootPath path =
  T.putStrLn (T.pack "Loading file " `T.append` T.pack path `T.append` T.pack "...")
  >> (Control.Concurrent.forkIO $ do
    contents <- load relPath path
    enqueue $ ServerLoadFileContents path contents)
  >> return ()
  where
    enqueue = STM.Messages.enqueueServerMessage messages
    relPath = rootPath `System.FilePath.combine` path

loadFilesContents :: STM.ServerMessages -> FilePath -> [FilePath] -> IO ()
loadFilesContents messages rootPath = mapM_ (Observer.WatchFile.loadFileContents messages rootPath)

loadFileModifications :: STM.ServerMessages -> STM.FileStore -> FilePath -> IO ()
loadFileModifications messages fileStore path =
  (Control.Concurrent.forkIO $ do
    --TODO: Get from fileStore
    maybeOldContents <- STM.FileStore.readFileContents fileStore path
    newContents <- load relPath path
    -- TODO: Generate a text operation for the changes
    return ())
  >> (return ())
  where
    rootPath = STM.FileStore.rootPath fileStore
    relPath = rootPath `System.FilePath.combine` path

load :: FilePath -> FilePath -> IO T.Text
load relPath path = do
  contents <- T.readFile relPath
  T.putStrLn (T.pack "...File loaded " `T.append` T.pack path)
  return contents
