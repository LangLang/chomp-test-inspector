module Observer.WatchExecutable (WatchExecutableHandle, forkObserver, killObserver, run, runEach) where

-- Standard modules
--import Prelude hiding (putStrLn)
--import Data.Text hiding (map, filter)
--import Data.Text.IO (putStrLn, hPutStrLn)
import System.INotify (INotify, EventVariety(..), Event(..), initINotify, killINotify, addWatch)
import Control.Monad (liftM)
import qualified System.Process
import qualified System.FilePath

-- Application modules
import Message
import qualified STM.Messages
import qualified STM.Messages as STM (ServerMessages)

-- Types
type WatchExecutableHandle = INotify

-- Run the asynchronous executable watch
forkObserver :: STM.ServerMessages -> FilePath -> IO (Maybe WatchExecutableHandle)
forkObserver messages execPath = liftM Just $ runINotify execPath
  where    
    -- Run inotify on the watch directory
    runINotify :: FilePath -> IO WatchExecutableHandle
    runINotify p = do
      inotify <- initINotify
      _       <- addWatch inotify masks p $ inotifyEvent messages
      return inotify
      where  
        masks = [ Modify, Attrib, Move, MoveSelf, Create, DeleteSelf ]

-- Stop the asynchronous file observer
killObserver :: WatchExecutableHandle -> IO ()
killObserver handle = killINotify handle

-- Handle inotify events (on files / directories) 
inotifyEvent :: STM.ServerMessages -> Event -> IO ()
inotifyEvent messages event = do
  case event of
    -- The executable was modified
    Modified False maybePath ->
      (putStrLn $ case maybePath of
        Just p -> "The executable '" ++ p ++ "' was modified."
        Nothing -> "The executable was modified.")
      >> executeAll
        
    -- The executable's attributes have changed
    Attributes False maybePath ->
      putStrLn $ case maybePath of
        Just p -> "The executable '" ++ p ++ "'s attributes has changed."   
        Nothing -> "The executable's attributes has changed."
       
    -- The executable was moved
    MovedSelf _ -> do
      putStrLn "The executable was moved."
    
    -- The executable has been newly created 
    Created False p ->
      (putStrLn $ "The executable '" ++ p ++ "' was created.")
      >> executeAll
      
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
  where
    enqueue = STM.Messages.enqueueServerMessage messages
     -- TODO: prevent this message from being repeatedly enqueued
     --       there should be something like an "enqueueOnce" function that prevents duplicates
    executeAll = enqueue ServerExecuteAll
    
run :: FilePath -> FilePath -> FilePath -> IO ()
run execPath rootPath path = do 
  processHandle <- System.Process.runProcess 
    execPath 
    [relPath] -- ["(" ++ relPath ++ " ->: (output -> (dir -> \"out/\")) -> _):_"]
    Nothing -- Working directory
    Nothing -- Environment
    Nothing -- stdin
    Nothing -- stdout
    Nothing -- stderr
  return ()
  where
    relPath = rootPath `System.FilePath.combine` path

runEach :: FilePath -> FilePath -> [FilePath] -> IO ()
runEach execPath rootPath paths = mapM_ (run execPath rootPath) paths  
