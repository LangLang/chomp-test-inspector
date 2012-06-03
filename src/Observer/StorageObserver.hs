{-# LANGUAGE OverloadedStrings, CPP #-}
module Observer.StorageObserver(FileObserver, forkFileObserver, killFileObserver) where

-- Standard modules
import Prelude hiding (putStrLn)
import Data.Text hiding (map, filter)
import Data.Text.IO (putStrLn, hPutStrLn)
import Data.String.Utils (endswith)
import qualified Data.STM.TList as STM (appendList)
import Control.Monad (liftM)
--import Control.Monad.Trans (liftIO)
import Control.Concurrent.STM (atomically)
import System.IO (stderr)
import System.IO.Error (try, ioeGetErrorType, IOErrorType)
import qualified System.Directory as Dir
import System.INotify (INotify, EventVariety(..), Event(..), initINotify, killINotify, addWatch)
#ifdef __GLASGOW_HASKELL__
import qualified GHC.IO.Exception as Exception
#endif

-- Application modules
import Message
import qualified STM.FileStore as STM (FileStore)
import qualified STM.FileStore hiding (FileStore)
import qualified STM.Messages as STM (Messages)
import qualified STM.Messages hiding (Messages)

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
      _ <- addWatch inotify masks watchPath $ inotifyEvent messages
      return $ Just inotify
  where
    isDots f = (endswith "/." f) || (endswith "/.." f) || (f == "..") || (f == ".")
    masks = [
        Modify,
        Attrib,
        --CloseWrite, 
        --CloseNoWrite,
        Move,
        MoveSelf, 
        Create,
        Delete,
        DeleteSelf, 
        OnlyDir
      ]
    
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

-- Handle inotify events (on files / directories) 
inotifyEvent :: STM.Messages -> Event -> IO ()
inotifyEvent messages e = do
  case e of
    -- A file was modified
    -- TODO: Load the changes (unless the modification was instigated by us in which case we're already up to date)
    Modified False maybePath -> do
      putStrLn $ (fromMaybeFilePath maybePath) `append` " was modified."
      case maybePath of
        Just p -> loadModifications p
        Nothing -> return ()
        
    -- A file's attributes have changed
    Attributes False maybePath -> do
      putStrLn $ "The file '" `append` (fromMaybeFilePath maybePath) `append` "'s attributes has changed."   
      -- TODO: notify the client if the file has become read-only (then gray out the editor)  
    
    -- A file was moved out of the watch path, so remove it from the file store
    MovedOut False p _ -> do
      putStrLn $ "'" `append` pack p `append` "' was moved out."
      unloadFile MovedOutFile p
      
    -- A file was moved into the watch path, so load it into the file store
    MovedIn False p _ -> do
      putStrLn $ "'" `append` pack p `append` "' was moved in." --TODO: use the cookie to check whether the file was actually renamed
      loadFile MovedInFile p
    
    -- The watch path was moved, so empty the storage
    MovedSelf _ -> do      
      putStrLn "The watched path was moved and hence no longer exists."
      unloadFiles MovedRootDirectory
      
    -- A new file was created, load it into the file store 
    Created False p -> do
      putStrLn $ "'" `append` pack p `append` "' was created."
      loadFile CreatedFile p
     
    -- A file in the watch path was deleted, so remove it from the file store
    Deleted False p -> do 
      putStrLn $ "'" `append` pack p `append` "' was deleted."
      unloadFile DeletedFile p
      
    -- The watch path was deleted, so remove all files from the file store
    DeletedSelf -> do
      putStrLn "The watched path was moved and hence no longer exists."
      unloadFiles DeletedRootDirectory
    
    -- The watch path was unmounted and is no longer accessible, so remove all files from the file store
    Unmounted -> do
      putStrLn "The watched path was unmounted and hence no longer exists."
      unloadFiles UnmountedRootDirectory 
    
    -- The inotify queue overflowed, remove all files from the file store
    -- TODO: Try to start from scratch by clearing the queues and reloading all files? 
    QOverflow -> do
      putStrLn "The queue overflowed, unload all the files."
      unloadFiles Error
    
    _ -> return ()

  where 
    fromMaybeFilePath :: Maybe FilePath -> Text
    fromMaybeFilePath = maybe "Unknown file" $ \filename -> "'" `append` pack filename `append` "'"
    
    enqueue = STM.Messages.enqueueMessage messages
    unloadFiles event = enqueue $ ReloadFiles event []
    reloadFiles event paths = enqueue $ ReloadFiles event paths
    loadFile event path = enqueue $ LoadFile event path
    unloadFile event path = return () -- TODO
    loadModifications path = return () :: IO ()-- TODO
