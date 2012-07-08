module Observer.WatchFile (loadFileContents, loadFilesContents) where

-- Standard modules
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Control.Concurrent
import qualified System.FilePath

-- Application modules
import Message
import qualified STM.Messages
import qualified STM.Messages as STM (ServerMessages)

loadFileContents :: STM.ServerMessages -> FilePath -> FilePath -> IO ()
loadFileContents messages rootPath path =
  T.putStrLn (T.pack "Loading file " `T.append` T.pack path `T.append` T.pack "...")
  >> (Control.Concurrent.forkIO $ do
    contents <- T.readFile relPath
    _ <- T.putStrLn (T.pack "...File loaded " `T.append` T.pack path)
    (enqueue $ ServerLoadFileContents path contents))
  >> return ()
  where
    enqueue = STM.Messages.enqueueServerMessage messages
    relPath = rootPath `System.FilePath.combine` path

loadFilesContents :: STM.ServerMessages -> FilePath -> [FilePath] -> IO ()
loadFilesContents messages rootPath = mapM_ (Observer.WatchFile.loadFileContents messages rootPath)