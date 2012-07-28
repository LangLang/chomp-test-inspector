module Observer.WatchFile (loadFileContents, loadFilesContents, loadFileModifications) where

-- Standard modules
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Control.Concurrent
import qualified System.FilePath
import qualified Data.Algorithm.Diff as Diff 

-- Supporting modules
-- https://github.com/timjb/haskell-operational-transformation
import qualified Control.OperationalTransformation.Text as OT

-- Application modules
import Message
import qualified STM.Messages as STM (ServerMessages)
import qualified STM.Messages
import qualified STM.FileStore as STM (FileStore)
import qualified STM.FileStore

loadFileContents :: STM.ServerMessages -> FilePath -> FilePath -> IO ()
loadFileContents messages rootPath path =
  T.putStrLn (T.pack "Loading file " `T.append` T.pack path `T.append` T.pack "...")
  >> (Control.Concurrent.forkIO $ do
    contents <- load relPath path
    enqueue $ ServerLoadFileContents path contents)
  >> return ()
  where
    enqueue = STM.Messages.enqueueServerMessage messages
    relPath = rootPath `System.FilePath.combine` path

loadFilesContents :: STM.ServerMessages -> FilePath -> [FilePath] -> IO ()
loadFilesContents messages rootPath = mapM_ (Observer.WatchFile.loadFileContents messages rootPath)

loadFileModifications :: STM.ServerMessages -> STM.FileStore -> FilePath -> IO ()
loadFileModifications messages fileStore path =
  (Control.Concurrent.forkIO $ do
    maybeOldContents <- STM.FileStore.readFileContents fileStore path
    newContents <- load relPath path
    case maybeOldContents of
      Nothing -> enqueue $ ServerLoadFileContents path newContents
      Just oldContents -> do
        let operations = generateOps oldContents newContents
        if length operations > 0 && (case operations of [OT.Retain _] -> False ; _ -> True) 
          then (enqueue $ ServerOperationalTransform path operations)
          else (T.putStrLn $ T.pack "No changes detected in the contents of the modified file, '" `T.append` (T.pack path) `T.append` (T.pack "'.")))
  >> (return ())
  where
    rootPath = STM.FileStore.rootPath fileStore
    relPath = rootPath `System.FilePath.combine` path
    enqueue = STM.Messages.enqueueServerMessage messages
    
    -- TODO: This is likely to be slow because text is being unpacked
    --       Also, probably using a more expensive diff algorithm than necessary
    --       See http://stackoverflow.com/questions/4611143/diffing-more-quickly
    --       and http://hackage.haskell.org/packages/archive/patience/0.1.1/doc/html/Data-Algorithm-Patience.html
    --       for possible alternatives
    
    -- TODO: Is cons the correct order here? (shouldn't be snoc?)
        
    generateOps :: T.Text -> T.Text -> [OT.Action]
    generateOps os ns =
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
    getCommon :: T.Text -> T.Text -> (Int, T.Text, T.Text)
    getCommon t0 t1 = match 0 
      where
        l = min (T.length t0) (T.length t1)
        match i = if (T.index t0 i) == (T.index t1 i) 
          then (if (i + 1) == l 
            then (l, T.drop l t0, T.drop l t1)
            else match (i + 1))
          else (i, T.drop i t0, T.drop i t1)

load :: FilePath -> FilePath -> IO T.Text
load relPath path = do
  contents <- T.readFile relPath
  T.putStrLn (T.pack "...File loaded " `T.append` T.pack path)
  return contents
