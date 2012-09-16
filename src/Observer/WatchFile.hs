module Observer.WatchFile (loadFileContents, loadFilesContents, loadFileModifications, applyOperation) where

-- Standard modules
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Control.Concurrent
import qualified System.FilePath as FilePath
import qualified Data.Algorithm.Diff as Diff
import qualified System.IO (stderr)

-- Supporting modules
-- https://github.com/timjb/haskell-operational-transformation
import qualified Control.OperationalTransformation.Text as OT
import qualified Control.OperationalTransformation.Server as OT

-- Application modules
import Message
import qualified FileStore
import FileStore (FileStore)
import qualified STM.Messages as STM (ServerMessages)
import qualified STM.Messages

loadFileContents :: STM.ServerMessages -> FilePath -> FilePath -> IO ()
loadFileContents messages rootPath path =
  T.putStrLn (T.pack "Loading file " `T.append` T.pack path `T.append` T.pack "...")
  >> (Control.Concurrent.forkIO $ do
    contents <- load relPath path
    enqueue $ ServerLoadFileContents path contents)
  >> return ()
  where
    enqueue = STM.Messages.enqueueServerMessage messages
    relPath = rootPath `FilePath.combine` path

loadFilesContents :: STM.ServerMessages -> FilePath -> [FilePath] -> IO ()
loadFilesContents messages rootPath = mapM_ (Observer.WatchFile.loadFileContents messages rootPath)

loadFileModifications :: STM.ServerMessages -> FileStore -> FilePath -> IO ()
loadFileModifications messages fileStore path =
  (Control.Concurrent.forkIO $ do
    maybeFileEntry <- FileStore.readFileStoreEntryIO fileStore path
    -- TODO: (IMPORTANT) increment revision numbers appropriately
    newContents <- load relPath path
    case maybeFileEntry of
      Nothing -> enqueue $ ServerLoadFileContents path newContents
      Just fileEntry -> do
        maybeCacheEntry <- FileStore.fileEntryCacheIO fileEntry
        case maybeCacheEntry of
          Nothing -> enqueue $ ServerLoadFileContents path newContents
          Just cacheEntry ->
            let oldContents = FileStore.cacheEntryContents cacheEntry
                actions = generateActions oldContents newContents
                revision = FileStore.revision $ FileStore.cacheEntryInfo cacheEntry 
            in
              if length actions > 0 && (case actions of [OT.Retain _] -> False ; _ -> True) 
                then apply messages fileStore path cacheEntry revision actions
                else (T.putStrLn $ T.pack "No changes detected in the contents of the modified file, '" `T.append` (T.pack path) `T.append` (T.pack "'.")))
  >> (return ())
  where
    rootPath = FileStore.rootPath fileStore
    relPath = rootPath `FilePath.combine` path
    enqueue = STM.Messages.enqueueServerMessage messages
    
    -- TODO: This is likely to be slow because text is being unpacked
    --       Also, probably using a more expensive diff algorithm than necessary
    --       See http://stackoverflow.com/questions/4611143/diffing-more-quickly
    --       and http://hackage.haskell.org/packages/archive/patience/0.1.1/doc/html/Data-Algorithm-Patience.html
    --       for possible alternatives
    
    -- TODO: Is cons the correct order here? (shouldn't be snoc?)
        
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
      
-- Apply operational transform to a file
applyOperation :: STM.ServerMessages -> FileStore -> FilePath -> OT.Revision -> [OT.Action] -> IO ()
applyOperation messages fileStore path revision actions = do
  maybeFileEntry <- FileStore.readFileStoreEntryIO fileStore path
  case maybeFileEntry of
    Nothing -> return () -- TODO: Error? False?
    Just fileEntry -> do
      maybeCacheEntry <- FileStore.fileEntryCacheIO fileEntry
      case maybeCacheEntry of
        Nothing -> return () -- TODO: Error? False?
        Just cacheEntry -> apply messages fileStore path cacheEntry revision actions
      
-- Apply OT actions to the file store's cache
apply :: STM.ServerMessages -> FileStore -> FilePath -> FileStore.FileCacheEntry -> OT.Revision -> [OT.Action] -> IO ()
apply messages fileStore path cacheEntry revision actions = 
  (T.putStrLn $ T.pack "Apply " `T.append` (T.pack $ show $ FileStore.cacheEntryInfo cacheEntry) `T.snoc` ' ' `T.append` (T.pack $ show revision) `T.snoc` ' ' `T.append` (T.pack $ show actions))
  >> let otResult = FileStore.applyOperationalTransform cacheEntry (revision, actions)
  in case otResult of
    Left errorMessage -> T.hPutStrLn System.IO.stderr $ T.pack "Operational transform failed: " `T.append` (T.pack $ show errorMessage)
    Right (actions', cacheEntry') ->
      -- Store updated state in the file store
      (FileStore.loadCacheIO fileStore path (FileStore.cacheEntryInfo cacheEntry') (FileStore.cacheEntryContents cacheEntry'))
      -- Add the operational transform to the message queue (to be broadcast to the clients)
      >> (enqueue $ ServerOperationalTransform path revision actions')
  where
    enqueue = STM.Messages.enqueueServerMessage messages
 

load :: FilePath -> FilePath -> IO T.Text
load relPath path = do
  contents <- T.readFile relPath
  T.putStrLn (T.pack "...File loaded " `T.append` T.pack path)
  return contents
