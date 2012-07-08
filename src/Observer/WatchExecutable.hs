--{-# LANGUAGE OverloadedStrings, CPP #-}
module Observer.WatchExecutable (WatchExecutableHandle) where

-- Standard modules
import System.INotify (INotify, EventVariety(..), Event(..), initINotify, killINotify, addWatch)
#ifdef __GLASGOW_HASKELL__
import qualified GHC.IO.Exception as Exception
#endif

-- Types
type WatchExecutableHandle = INotify

-- Run the asynchronous executable watch
forkObserver :: FilePath -> IO (Maybe WatchExecutableHandle)
forkObserver execPath = liftM Just $ runINotify execPath
  where    
    -- Run inotify on the watch directory
    runINotify :: FilePath -> IO WatchExecutableHandle
    runINotify execPath = do
      inotify <- initINotify
      _       <- addWatch inotify masks execPath $ inotifyEvent execPath messages
      return inotify
      where  
        masks = [ Modify, Attrib, Move, MoveSelf, Create, DeleteSelf ]

-- Stop the asynchronous file observer
killObserver :: WatchExecutableHandle -> IO ()
killObserver handle = killINotify handle

-- Handle inotify events (on files / directories) 
inotifyEvent :: Event -> IO ()
inotifyEvent event = do
  case event of
    -- The executable was modified
    Modified False maybePath -> do
      case maybePath of
        Just path -> "The executable '" `append` pack p `append` "' was modified."
        Nothing -> "The executable was modified."
        
    -- The executable's attributes have changed
    Attributes False maybePath -> do
      case maybePath of
        Just p -> putStrLn $ "The executable '" `append` pack p `append` "'s attributes has changed."   
        Nothing -> putStrLn $ "The executable's attributes has changed."
       
    -- The executable was moved
    MovedSelf _ -> do
      putStrLn "The executable was moved."
    
    -- The executable has been newly created 
    Created False p -> do
      putStrLn $ "The executable '" `append` pack p `append` "' was created."
     
    -- The executable was deleted
    --Deleted False p -> do 
    --  putStrLn $ "The executable '" `append` pack p `append` "' was deleted."
    --  unloadFile DeletedFile p
      
    -- The executable was deleted
    DeletedSelf -> do
      putStrLn "The executable was deleted."
    
    -- The executable's path was unmounted and is no longer accessible
    Unmounted -> do
      putStrLn "The executable's path was unmounted and hence no longer exists."
    
    -- The inotify queue overflowed
    QOverflow -> do
      putStrLn "The watch queue for the executable overflowed."
    
    _ -> return ()
