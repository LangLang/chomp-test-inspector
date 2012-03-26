module STM.FileStore (FileStore, fromPaths) where

-- Standard modules
import Control.Monad (liftM, foldM)
import Control.Concurrent.STM (STM, atomically)
import Data.STM.TList (TList)
import qualified Data.STM.TList as TList

-- Application modules
import FileStore

type FileStore = TList FileInfo
fromPaths :: [FilePath] -> STM FileStore
fromPaths = liftM fst . TList.fromList

--fromPaths :: [FilePath] -> IO FileStore
--fromPaths paths = atomically $ do
  --list <- TList.empty
  --foldM (flip TList.cons) list rpaths
  --return $ liftM fst $
  --where
  --  rpaths = reverse paths





























