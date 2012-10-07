module Observer.WatchFile (loadFileContents, loadFilesContents, loadFileModifications, mergeOperation) where

-- Standard modules
import qualified Data.Text as T
import qualified Data.Text.IO as T
-- import Data.Functor
import qualified Control.Concurrent
import Control.Monad
-- import Control.Monad.Trans.Maybe
import qualified System.FilePath as FilePath
import qualified Data.Algorithm.Diff as Diff
import qualified System.IO as IO

-- Supporting modules
-- https://github.com/timjb/haskell-operational-transformation
import qualified Control.OperationalTransformation.Text as OT

-- Application modules
import IOUtil
import Message
import qualified OTServer as OT
import qualified FileStore as FS
import FileStore (FileStore)
import qualified STM.FileStore as STM.FS
import qualified STM.Messages as STM (ServerMessages)
import qualified STM.Messages

loadFileContents :: STM.ServerMessages -> FilePath -> FilePath -> IO ()
loadFileContents messages rootPath path = do
  T.putStrLn (T.pack "Loading file " `T.append` T.pack path `T.append` T.pack "...")
  Control.Concurrent.forkIO $ do
    contents <- load relPath path
    enqueue $ ServerLoadFileContents path contents
  >> return ()  
  where
    enqueue = STM.Messages.enqueue messages <=< stampServerMessage
    relPath = rootPath `FilePath.combine` path

loadFilesContents :: STM.ServerMessages -> FilePath -> [FilePath] -> IO ()
loadFilesContents messages rootPath = mapM_ (Observer.WatchFile.loadFileContents messages rootPath)

loadFileModifications :: STM.ServerMessages -> FileStore -> FilePath -> IO ()
loadFileModifications messages fileStore path = do
  let rootPath = FS.rootPath fileStore
      relPath = rootPath `FilePath.combine` path
  -- Run a separate thread to load the file contents and perform a diff 
  Control.Concurrent.forkIO $ do
    {- Test whether the file was written to and could be safely ignored 
    maybeCacheEntry     <- runMaybeT $ 
      MaybeT (FS.readFileStoreEntryIO fileStore path) 
      >>= MaybeT . FS.fileEntryCacheIO
    -}
    
    -- In order to avoid the file being changed any time during this function, we open the file
    -- in write mode and keep it open for the duration of the function (effectively putting a
    -- write lock on it)
    -- We use a closed counter to test whether the file needs to be tested for modifications, or if
    -- this signal was simply the result of closing the file handle ourselves
       
    maybeMaybeCacheEntry <- FS.modifyTestCacheEntryIO fileStore path decTestClosedCounter
    case maybeMaybeCacheEntry of
      Nothing                -> do -- The file's contents is not currently in the cache
        load relPath path
        >>= (enqueue . ServerLoadFileContents path)
      Just (Right _)  ->           -- (closed counter was > 0)
                                   -- Don't load the file if the closed counter has been decremented 
        return () 
      Just (Left cacheEntry) -> do -- (closed counter was == 0)
                                   -- Open the file using a write a lock and test if it has been modified
        (h, storedContents) <- loadWriteLocked relPath path
        let cachedContents  = FS.cacheEntryContents cacheEntry
            actions         = generateActions cachedContents storedContents 
            op              = OT.TextOperation actions 
        cacheEntry''' <- if length actions == 0 || (case actions of [OT.Retain _] -> True ; _ -> False) 
          then do 
            putStrLn $ "No changes detected in the contents of the modified file, '" ++ path ++ "'."
            return cacheEntry
          else
            let eitherCacheEntry' = FS.mergeAtContentsRevision cacheEntry op
            in case eitherCacheEntry' of
              Left err -> do
                putStrLn $ "Merge failed: " ++ err ++ " >>>"
                putStrLn $ "\t>>> cache entry info: " ++ show (FS.cacheEntryInfo cacheEntry)
                putStrLn $ "\t>>> cache entry contents: " ++ show (FS.cacheEntryContents cacheEntry)
                return cacheEntry
              Right cacheEntry' -> do
                eitherCacheEntry'' <- FS.updateFileContentsIO fileStore path cacheEntry'
                case eitherCacheEntry'' of
                  Left err -> do
                    putStrLn $ "Update file contents failed: " ++ err ++ " >>>"
                    putStrLn $ "\t>>> cache entry info: " ++ show (FS.cacheEntryInfo cacheEntry')
                    putStrLn $ "\t>>> cache entry contents: " ++ show (FS.cacheEntryContents cacheEntry')
                    return cacheEntry'
                  Right cacheEntry'' -> let
                    contents'                     = FS.cacheEntryContents cacheEntry''
                    fi'                           = FS.cacheEntryInfo cacheEntry''
                    ops'                          = FS.operations fi'  
                    (OT.TextOperation actions'):_ = ops'
                    opId                          = "n/a" -- It is not necessary to generate a random id for server-generated operations
                                                          -- (because they do not require acknowledgement)
                    in do
                      -- Test if the file stored on disk already corresponds to the newly cached contents 
                      _ <- if (contents' /= storedContents) 
                        then do
                          putStr $ "\t...Writing changes to '" ++ path ++ "'"
                          -- TODO: putStrLn hIsOpen ? 
                          writeToDisk h contents'
                          putStrLn " (Done)"
                        else do
                          putStrLn $ "\t...No changes to write to '" ++ path ++ "'"
                      enqueue $ ServerOperationalTransform path (FS.opsRevision fi' - 1) actions' opId
                      return cacheEntry''
        _ <- FS.storeCacheEntryIO fileStore path $ incClosedCounter cacheEntry'''
        IO.hClose h
  >> return ()
  where
    enqueue = STM.Messages.enqueue messages <=< stampServerMessage
    
    incClosedCounter :: FS.FileCacheEntry -> FS.FileCacheEntry 
    incClosedCounter (STM.FS.FileCacheEntry fi@(FS.FileInfo {FS.closedCounter=cc}) fc) =
      STM.FS.FileCacheEntry (fi {FS.closedCounter = cc + 1}) fc
    
    decTestClosedCounter :: FS.FileCacheEntry -> Maybe FS.FileCacheEntry
    decTestClosedCounter (STM.FS.FileCacheEntry fi@(FS.FileInfo {FS.closedCounter=cc}) fc) =
      if cc > 0
        then Just $ STM.FS.FileCacheEntry (fi {FS.closedCounter = cc - 1}) fc
        else Nothing
    
    -- TODO: This is likely to be slow because text is being unpacked
    --       Also, probably using a more expensive diff algorithm than necessary
    --       See http://stackoverflow.com/questions/4611143/diffing-more-quickly
    --       and http://hackage.haskell.org/packages/archive/patience/0.1.1/doc/html/Data-Algorithm-Patience.html
    --       for possible alternatives
    generateActions :: T.Text -> T.Text -> [OT.Action]
    generateActions os ns =
      if T.null ns
        then [OT.Delete $ T.length os]
        else if T.null os
          then [OT.Insert ns]
          else let (l, ros, rns) = getCommon os ns
            in if l > 0
              then (OT.Retain l):(generateDiffOps $ getGroupedDiffText ros rns)
              else (generateDiffOps $ getGroupedDiffText  ros rns)
      where
        getGroupedDiffText t0 t1 = Diff.getGroupedDiff (T.unpack t0) (T.unpack t1)
    
    generateDiffOps :: [(Diff.DI, String)] -> [OT.Action]
    generateDiffOps []               = []
    generateDiffOps ((Diff.B, t):xs) = (OT.Retain $ length t):(generateDiffOps xs)
    generateDiffOps ((Diff.F, t):xs) = (OT.Delete $ length t):(generateDiffOps xs)
    generateDiffOps ((Diff.S, t):xs) = (OT.Insert $ T.pack t):(generateDiffOps xs)

    -- It is possible that function similar to Text.spanBy used for this could be much faster
    -- TODO: use  (which I missed previously) 
    getCommon :: T.Text -> T.Text -> (Int, T.Text, T.Text)
    getCommon t0 t1 = case T.commonPrefixes t0 t1 of
      Nothing -> (0, t0, t1)
      Just (prefix, r0, r1) -> (T.length prefix, r0, r1)
    
    writeToDisk :: IO.Handle -> T.Text -> IO ()
    writeToDisk h text = do
      IO.hSeek h IO.AbsoluteSeek 0
      T.hPutStr h text
      IO.hSetFileSize h =<< IO.hTell h 

-- Merge an operational transform into a file's list of operational transforms to be applied
mergeOperation :: FileStore -> FilePath -> OT.Revision -> [OT.Action] -> OperationId -> IO (Either String ServerMessage)
mergeOperation fileStore path revision actions opId = do
  maybeFileEntry <- FS.readFileStoreEntryIO fileStore path
  case maybeFileEntry of
    Nothing        -> 
      -- TODO: Implement a more sophisticated solution for this case (add a placeholder into the filestore) 
      return $ Left $ "The file `" ++ path ++ "` could not be located in the file store." 
    Just fileEntry -> do
      maybeCacheEntry <- FS.fileEntryCacheIO fileEntry
      case maybeCacheEntry of
        Nothing         -> 
          -- TODO: Implement a more sophisticated solution for this case (add a placeholder into the filestore) 
          return $ Left $ "The file `" ++ path ++ "`'s contents has not been loaded into the file store."
        Just cacheEntry -> 
          --apply fileStore path cacheEntry revision actions opId
          let fi = FS.cacheEntryInfo cacheEntry in
          case OT.mergeAtRevision (FS.operations fi) revision (OT.TextOperation actions) of
            Left err   -> return $ Left err
            Right ops' ->
              let (OT.TextOperation actions'):_ = ops' 
                  fi' = fi {FS.operations = ops'}
              in
                FS.storeCacheIO fileStore path fi' (FS.cacheEntryContents cacheEntry)
                >> (return $ Right $ ServerOperationalTransform path (FS.opsRevision fi' - 1) actions' opId)

-- Reads the file contents 
load :: FilePath -> FilePath -> IO T.Text
load relPath path = do
  contents <- T.readFile relPath
  putStrLn $ "\t...File loaded " ++ path
  return contents

-- Reads the file contents and returns an open (write-locked) file handle and the contents of the file)
loadWriteLocked :: FilePath -> FilePath -> IO (IO.Handle, T.Text)
loadWriteLocked relPath path = do
  h <- IO.openFile relPath IO.ReadWriteMode
  contents <- hGetContentsOpen h
  putStrLn $ "\t...File loaded " ++ path ++ " (with write lock)"
  return (h, contents)
  where 
    -- We cannot use hGetContents directly, because hGetContents automatically closes the handle 
    -- (which causes the write lock to be lost) 
    hGetContentsOpen :: IO.Handle -> IO T.Text
    hGetContentsOpen h = do
      -- Read the first line (to prevent a newline being added before the first line)
      firstLine <- T.hGetLine h
      isEOF <- IO.hIsEOF h
      -- Read all remaining lines in the file
      if (not isEOF)
        then foldUntilIO (T.append . (`T.snoc` '\n')) firstLine (IO.hIsEOF h) (T.hGetLine h)
        else return firstLine

