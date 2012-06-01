{-# LANGUAGE OverloadedStrings, CPP #-}
module Observer.StorageObserver(FileObserver, forkFileObserver, killFileObserver) where

-- Standard modules
import Prelude hiding (putStrLn)
import Data.Text hiding (map, filter)
import Data.Text.IO (putStrLn, hPutStrLn)
import Data.String.Utils (endswith)
import qualified Data.STM.TList as STM (append, appendList)
import Control.Monad (liftM)
--import Control.Monad.Trans (liftIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM as STM (writeTChan)
import System.IO (stderr)
import System.IO.Error (try, ioeGetErrorType, IOErrorType)
import qualified System.Directory as Dir
import System.INotify (INotify, EventVariety(..), Event(..), initINotify, killINotify, addWatch)
#ifdef __GLASGOW_HASKELL__
import qualified GHC.IO.Exception as Exception
#endif

-- Application modules
import qualified Message (Message(..))
import qualified STM.FileStore as STM (FileStore)
import qualified STM.Messages as STM (Messages)

-- Types
type FileObserver = INotify

-- Run the asynchronous file observer
forkFileObserver :: FilePath -> STM.FileStore -> STM.Messages -> IO (Maybe FileObserver)
forkFileObserver watchPath fileStore messages = do 
  filesOrError <- try $ do
    -- Get the initial contents of the directory being watched
    initialFiles <- (liftM $ filter $ not . isDots) $ Dir.getDirectoryContents watchPath
    atomically $ STM.appendList fileStore initialFiles
  case filesOrError of
    Left e -> do
      case generateErrorMessage $ ioeGetErrorType e of 
        Just message -> do
          hPutStrLn stderr $ pack message
          return Nothing
        Nothing -> ioError e
    Right _ -> do
      -- Setup inotify to watch the directory
      inotify <- initINotify 
      _ <- addWatch inotify [Modify, Create, Delete, Move] watchPath $ sourceFileChanged fileStore messages
      return $ Just inotify
  where
    isDots f = (endswith "/." f) || (endswith "/.." f) || (f == "..") || (f == ".")
    
    -- Generate a message from an IO error  
    generateErrorMessage :: IOErrorType -> Maybe String
#ifdef __GLASGOW_HASKELL__
    generateErrorMessage errorType = case errorType of
      -- GHC only:
      Exception.NoSuchThing -> Just $ "The path supplied `" ++ watchPath ++ "` does not exist."
      Exception.PermissionDenied -> Just $ "Permission to read the the path `" ++ watchPath ++ "` was denied."      
      Exception.InvalidArgument -> Just $ "The path supplied `" ++ watchPath ++ "` is not a valid directory name."
      Exception.InappropriateType -> Just $ "The path supplied `" ++ watchPath ++ "` refers to a non-directory object."
      _ -> Nothing
#else
    generateErrorMessage errorType = Nothing
#endif

-- Stop the asynchronous file observer
killFileObserver :: FileObserver -> IO ()
killFileObserver fileObserver = killINotify fileObserver

-- Server actions
--serverLoadFile :: FileInfo -> IO () 
--serverLoadFile fileInfo = atomically $ Messages

sourceFileChanged :: STM.FileStore -> STM.Messages -> Event -> IO ()
sourceFileChanged fileStore messages e = do
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
      _ <- atomically $ do
        _ <- STM.append fileStore p
        STM.writeTChan messages $ Message.LoadFile p 
      return ()
    Deleted False p -> putStrLn $ "'" `append` pack p `append` "' was deleted."
    DeletedSelf -> putStrLn "The watched path was moved and hence no longer exists."
    Unmounted -> putStrLn "The watched path was unmounted and hence no longer exists."
    QOverflow -> putStrLn "TODO: The queue overflowed, resend all the files."
    _ -> return ()
  where
    fromMaybeFilePath :: Maybe FilePath -> Text
    fromMaybeFilePath = maybe "Unknown file" $ \filename -> "'" `append` pack filename `append` "'"
