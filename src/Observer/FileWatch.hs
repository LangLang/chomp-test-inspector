module Observer.FileWatch (loadFileContents) where

-- Standard modules
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Control.Concurrent

-- Application modules
import Message
import qualified STM.Messages
import qualified STM.Messages as STM (ServerMessages)

loadFileContents :: STM.ServerMessages -> FilePath -> IO ()
loadFileContents messages path = 
  T.putStrLn (T.pack "File loaded: " `T.append` T.pack path)
  >> (Control.Concurrent.forkIO $ do
    contents <- T.readFile path
    _ <- T.putStrLn (T.pack "File loaded: " `T.append` T.pack path `T.append` T.pack "\n" `T.append` contents)
    (enqueue $ ServerLoadFileContents path contents))
  >> return ()
  where
    enqueue = STM.Messages.enqueueServerMessage messages
