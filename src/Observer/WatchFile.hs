module Observer.WatchFile (loadFileContents, loadFilesContents, loadFileModifications, applyOperation) where

-- Standard modules
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Control.Concurrent
import Control.Monad ((<=<))
import qualified System.FilePath as FilePath
import qualified Data.Algorithm.Diff as Diff

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
    enqueue = STM.Messages.enqueue messages <=< stampServerMessage
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
                then do
                  let opId = "n/a" -- It is not necessary to generate a random id for server-generated operations (because they do not require acknowledgement)
                  otResult <- apply fileStore path cacheEntry revision actions opId
                  case otResult of
                    Left err -> T.putStrLn $ T.pack err
                    Right message -> enqueue message 
                else (T.putStrLn $ T.pack "No changes detected in the contents of the modified file, '" `T.append` (T.pack path) `T.append` (T.pack "'.")))
  >> (return ())
  where
    rootPath = FileStore.rootPath fileStore
    relPath = rootPath `FilePath.combine` path
    enqueue = STM.Messages.enqueue messages <=< stampServerMessage
    
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
applyOperation :: FileStore -> FilePath -> OT.Revision -> [OT.Action] -> OperationId -> IO (Either String ServerMessage) 
applyOperation fileStore path revision actions opId = do
  maybeFileEntry <- FileStore.readFileStoreEntryIO fileStore path
  case maybeFileEntry of
    Nothing -> 
      -- TODO: Implement a more sophisticated solution for this case 
      return $ Left $ "The file `" ++ path ++ "` could not be located in the file store." 
    Just fileEntry -> do
      maybeCacheEntry <- FileStore.fileEntryCacheIO fileEntry
      case maybeCacheEntry of
        Nothing -> 
          -- TODO: Implement a more sophisticated solution for this case 
          return $ Left $ "The file `" ++ path ++ "`'s contents has not been loaded into the file store."
        Just cacheEntry -> 
          apply fileStore path cacheEntry revision actions opId
      
-- Apply OT actions to the file store's cache
apply :: FileStore -> FilePath -> FileStore.FileCacheEntry -> OT.Revision -> [OT.Action] -> OperationId -> IO (Either String ServerMessage)  
apply fileStore path cacheEntry revision actions opId = 
  case FileStore.applyOperationalTransform cacheEntry (revision, actions) of
    Left errorMessage -> return $ Left $ "Operational transform failed: " ++ show errorMessage
    Right (actions', cacheEntry') ->
      -- Store updated state in the file store
      let fileInfo' = FileStore.cacheEntryInfo cacheEntry'
          fileContents' = FileStore.cacheEntryContents cacheEntry' in
      (FileStore.loadCacheIO fileStore path fileInfo' fileContents')
      -- Add the operational transform to the message queue (to be broadcast to the clients)
      >> (return $ Right $ ServerOperationalTransform path (FileStore.revision fileInfo' - 1) actions' opId)

load :: FilePath -> FilePath -> IO T.Text
load relPath path = do
  contents <- T.readFile relPath
  T.putStrLn (T.pack "\t...File loaded " `T.append` T.pack path)
  return contents
