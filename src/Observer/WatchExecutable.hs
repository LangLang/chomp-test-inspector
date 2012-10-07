module Observer.WatchExecutable (WatchExecutableHandle, forkObserver, killObserver, run, runEach) where

-- Standard modules
--import Prelude hiding (putStrLn)
--import Data.Text hiding (map, filter)
--import Data.Text.IO (putStrLn, hPutStrLn)
import System.INotify (INotify, EventVariety(..), Event(..), initINotify, killINotify, addWatch)
import Control.Monad (liftM, (<=<))
import Control.Concurrent as C
import qualified System.IO
import qualified System.Process as P
--import qualified System.Exit
--import qualified System.FilePath
import System.FilePath ((</>))

-- Application modules
import Message
import qualified STM.Messages
import qualified STM.Messages as STM (ServerMessages)
import IOUtil (foreverWhileIO)

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
    enqueue = STM.Messages.enqueue messages <=< stampServerMessage
     -- TODO: prevent this message from being repeatedly enqueued
     --       there should be something like an "enqueueOnce" function that prevents duplicates
    executeAll = enqueue ServerExecuteAll

-- Run the executable, adding all log messages to the message queue
run :: STM.ServerMessages -> FilePath -> FilePath -> FilePath -> IO ()
run serverMessages execPath rootPath path = do
  (_, Just hStdOut, Just hStdErr, hProcess) <- 
    P.createProcess (P.proc execPath [relPath, outputPath])
    { P.std_out = P.CreatePipe, P.std_err = P.CreatePipe }
  readProcessStreams serverMessages hProcess path hStdOut hStdErr
  where
    relPath = rootPath </> path
    outputPath = relPath ++ ".output"

-- Run the executable on a list of file paths
runEach :: STM.ServerMessages -> FilePath -> FilePath -> [FilePath] -> IO ()
runEach serverMessages execPath rootPath paths = mapM_ (run serverMessages execPath rootPath) paths

readProcessStreams :: STM.ServerMessages -> P.ProcessHandle -> FilePath -> System.IO.Handle -> System.IO.Handle -> IO ()
readProcessStreams serverMessages hProcess outputPath hStdOut hStdErr = do
  -- Note: The -threaded flag is needed to avoid blocking all threads while running this function
  --      (See System.Process.readProcess)
   
  -- TODO: Handle the case where the process doesn't terminate (manually signal process to be terminated)
   
  -- TODO: Needed?
  -- hSetBinaryMode hStdOut False
  -- hSetBinaryMode hStdErr False
  
  -- Set the buffering mode to line buffering by default
  -- TODO: Not sure whether this makes any difference on the read end of the pipe.
  --       Ideally this buffering mode should probably also be set inside the subprocess.
  System.IO.hSetBuffering hStdOut System.IO.LineBuffering
  System.IO.hSetBuffering hStdErr System.IO.LineBuffering
  
  -- Fork a thread to listen for output on the two handles (stdout + stderr)
  logMessage LogStart
  
  semStdOut <- newEmptyMVar
  _ <- forkIO $
    (foreverWhileIO (liftM not $ System.IO.hIsEOF hStdOut) $ do 
      line <- System.IO.hGetLine hStdOut
      logMessage $ LogInfo line)
    >> (putMVar semStdOut ()) 
  
  semStdErr <- newEmptyMVar
  _ <- forkIO $
    (foreverWhileIO (liftM not $ System.IO.hIsEOF hStdErr) $ do 
      line <- System.IO.hGetLine hStdErr
      logMessage $ LogError line)
    >> (putMVar semStdErr ())
  
  -- Block until both stdout and stderr are closed
  _ <- 
    (takeMVar semStdOut)
    >> (takeMVar semStdErr)
    >> (System.IO.hClose hStdOut) 
    >> (System.IO.hClose hStdErr)
  
  -- Get the exit code for the process
  exitCode <- P.waitForProcess hProcess
  logMessage $ LogEnd exitCode
  where
    logMessage :: ProcessLog -> IO ()
    logMessage msg = (STM.Messages.enqueue serverMessages <=< stampServerMessage) $ ServerNotify $ ProcessMessage outputPath msg 
  
