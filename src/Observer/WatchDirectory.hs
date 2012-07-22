{-# LANGUAGE OverloadedStrings, CPP #-}
module Observer.WatchDirectory(WatchDirectoryHandle, forkObserver, killObserver) where

-- Standard modules
import Prelude hiding (putStrLn)
import Data.Text hiding (map, filter)
import Data.Text.IO (putStrLn, hPutStrLn)
import Data.String.Utils (endswith)
--import Control.Monad.Trans (liftIO)
import Control.Monad (liftM)
import System.IO (stderr)
import System.IO.Error (ioeGetErrorType, IOErrorType)
import Control.Exception (try)
import System.INotify (INotify, EventVariety(..), Event(..), initINotify, killINotify, addWatch)
import System.Directory (getDirectoryContents) 
#ifdef __GLASGOW_HASKELL__
import qualified GHC.IO.Exception as Exception
#endif

-- Application modules
import Message
import FileStore
import qualified STM.FileStore
import qualified STM.FileStore as STM (FileStore)
import qualified STM.Messages
import qualified STM.Messages as STM (ServerMessages)
--import qualified Observer.WatchFile

-- Types
type WatchDirectoryHandle = INotify

-- Run the asynchronous directory watch
forkObserver :: STM.FileStore -> STM.ServerMessages -> IO (Maybe WatchDirectoryHandle)
forkObserver fileStore messages = do
  let rootPath = STM.FileStore.rootPath fileStore
  -- Try to load files in the watch directory
  errorOrFiles <- try $ listAllFiles rootPath 
  -- Either fail gracefully if reading the path failed, or start watching the directory
  case errorOrFiles of
    Left e -> do
      -- Clear the file store and return no observer
      STM.FileStore.clear fileStore
      case generateIOErrorMessage rootPath $ ioeGetErrorType e of 
        Just message -> do
          hPutStrLn stderr $ pack message
          return Nothing
        Nothing -> ioError e
    Right files ->
      -- Load all files and return the directory's observer
      -- TODO: is there an inotify event we can watch to enqueue this message instead
      (enqueue $ ServerReloadFiles WatchInstalled files) 
      >> (liftM Just $ runINotify rootPath)
  where
    -- Enqueue a server message
    enqueue = STM.Messages.enqueueServerMessage messages
    
    -- Run inotify on the watch directory
    runINotify :: FilePath -> IO WatchDirectoryHandle
    runINotify rootPath = do
      inotify <- initINotify
      _       <- addWatch inotify masks rootPath $ inotifyEvent rootPath messages
      return inotify
      where  
        masks = [ Modify, Attrib, Move, MoveSelf, Create, Delete, DeleteSelf, OnlyDir ]
    
    -- Generate a message from an IO error  
    generateIOErrorMessage :: FilePath -> IOErrorType -> Maybe String
#ifdef __GLASGOW_HASKELL__
    generateIOErrorMessage rootPath errorType = case errorType of
      -- GHC only:
      Exception.NoSuchThing -> Just $ "The path supplied `" ++ rootPath ++ "` does not exist."
      Exception.PermissionDenied -> Just $ "Permission to read the the path `" ++ rootPath ++ "` was denied."      
      Exception.InvalidArgument -> Just $ "The path supplied `" ++ rootPath ++ "` is not a valid directory name."
      Exception.InappropriateType -> Just $ "The path supplied `" ++ rootPath ++ "` refers to a non-directory object."
      _ -> Nothing
#else
    generateErrorMessage errorType = Nothing
#endif

-- Stop the asynchronous file observer
killObserver :: WatchDirectoryHandle -> IO ()
killObserver handle = killINotify handle

-- Handle inotify events (on files / directories) 
inotifyEvent :: FilePath -> STM.ServerMessages -> Event -> IO ()
inotifyEvent rootPath messages event = do
  case event of
    -- A file was modified
    -- TODO: Load the changes (unless the modification was instigated by us in which case we're already up to date)
    Modified False maybePath -> do
      putStrLn $ case maybePath of
        Just p -> "'" `append` pack p `append` "' was modified."
        Nothing -> "An unknown was modified."
      case maybePath of
        Just p -> loadModifications p
        Nothing -> return ()
        
    -- A file's attributes have changed
    Attributes False maybePath -> do
      putStrLn $ case maybePath of
        Just p -> "The file '" `append` pack p `append` "'s attributes has changed."
        Nothing -> "An unknown file's attributes has changed."
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
      putStrLn "The watched path was moved."
      movedWatchPath
    
    -- A new file was created, load it into the file store 
    Created False p -> do
      putStrLn $ "'" `append` pack p `append` "' was created."
      loadFile CreatedFile p
     
    -- A file in the watch path was deleted, so remove it from the file store
    Deleted False p -> do 
      putStrLn $ "'" `append` pack p `append` "' was deleted."
      unloadFile DeletedFile p
      
    -- The watch path was deleted, so remove all files from the file store
    -- TODO: (BUG) This event does not appear to trigger at the moment. 
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
      putStrLn $ "The watch queue for '" `append` pack rootPath `append` "' overflowed, unload all the files."
      unloadFiles Error
    
    _ -> return ()

  where
    enqueue = STM.Messages.enqueueServerMessage messages
    unloadFiles e = enqueue $ ServerReloadFiles e []
    movedWatchPath = do
      errorOrFiles <- try $ listAllFiles rootPath :: IO (Either IOError [FileInfo])
      case errorOrFiles of
        Left _      -> enqueue $ ServerReloadFiles MovedOutRootDirectory [] 
        Right files -> enqueue $ ServerReloadFiles RestoredRootDirectory files
    loadFile e path = enqueue $ ServerLoadFile e path      
    unloadFile e path = enqueue $ ServerUnloadFile e path
    loadModifications path = return () :: IO () -- TODO

-- List all files in the watch directory
listAllFiles :: FilePath -> IO [FileInfo]
listAllFiles rootPath = (liftM $ filter $ not . isDots) $ getDirectoryContents rootPath
  where
    isDots f = (endswith "/." f) || (endswith "/.." f) || (f == "..") || (f == ".")
