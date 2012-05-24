{-# LANGUAGE OverloadedStrings #-}
module FileObserver(FileObserver, forkFileObserver, killFileObserver) where

-- Standard modules
import Prelude hiding (putStrLn)
import Data.Text hiding (map, filter)
import Data.Text.IO (putStrLn)
import Data.String.Utils (endswith)
import qualified Data.STM.TList as STM (append, appendList)
import Control.Monad (liftM)
--import Control.Monad.Trans (liftIO)
--import Control.Exception (try)
import Control.Concurrent.STM (atomically)
import qualified System.Directory as Dir
import System.INotify (INotify, EventVariety(..), Event(..), initINotify, killINotify, addWatch)

-- Application modules
import qualified STM.FileStore as STM (FileStore)

-- Types
type FileObserver = INotify

-- Run the asynchronous file observer
forkFileObserver :: FilePath -> STM.FileStore -> IO FileObserver
forkFileObserver watchPath fileStore = do
  -- Get the initial contents of the directory being watched
  initialFiles <- (liftM $ filter $ not . isDots) $ Dir.getDirectoryContents watchPath
  _ <- atomically $ STM.appendList fileStore initialFiles
  -- Setup inotify to watch the directory
  inotify <- initINotify 
  _ <- addWatch inotify [Modify, Create, Delete, Move] watchPath $ sourceFileChanged fileStore
  return inotify
  where
    isDots f = (endswith "/." f) || (endswith "/.." f) || (f == "..") || (f == ".")

-- Stop the asynchronous file observer
killFileObserver :: FileObserver -> IO ()
killFileObserver fileObserver = killINotify fileObserver

-- Server actions
--serverLoadFile :: FileInfo -> IO () 
--serverLoadFile fileInfo = atomically $ Messages

sourceFileChanged :: STM.FileStore -> Event -> IO ()
sourceFileChanged fileStore e = do
  case e of
    Modified False p -> putStrLn $ (fromMaybeFilePath p) `append` " was modified."
    MovedOut False p _ -> do
      putStrLn $ "'" `append` pack p `append` "' was moved out."
      --readMVar fileStore
      do
        --n <- atomically $ TList.start fileStore
        return ()
    MovedIn False p _ -> putStrLn $ "'" `append` pack p `append` "' was moved in." --TODO: use the cookie to check whether the file was actually renamed
    MovedSelf _ -> putStrLn "The watched path was moved and hence no longer exists."
    Created False p -> do
      putStrLn $ "'" `append` pack p `append` "' was created."
      _ <- atomically $ STM.append fileStore p
      -- TODO: Add a load event to the outgoing message queue
      return ()
    Deleted False p -> putStrLn $ "'" `append` pack p `append` "' was deleted."
    DeletedSelf -> putStrLn "The watched path was moved and hence no longer exists."
    Unmounted -> putStrLn "The watched path was unmounted and hence no longer exists."
    QOverflow -> putStrLn "TODO: The queue overflowed, resend all the files."
    _ -> return ()
  where
    fromMaybeFilePath :: Maybe FilePath -> Text
    fromMaybeFilePath = maybe "Unknown file" $ \filename -> "'" `append` pack filename `append` "'"
