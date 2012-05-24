module STM.FileStore (FileStore, newIO, fromPaths) where

-- Standard modules
import Control.Monad (liftM)
import Control.Concurrent.STM (STM)
import Data.STM.TList (TList, emptyIO)
import qualified Data.STM.TList as TList

-- Application modules
import FileStore

type FileStore = TList FileInfo

newIO :: IO FileStore
newIO = emptyIO

fromPaths :: [FilePath] -> STM FileStore
fromPaths = liftM fst . TList.fromList

--fromPaths :: [FilePath] -> IO FileStore
--fromPaths paths = atomically $ do
  --list <- TList.empty
  --foldM (flip TList.cons) list rpaths
  --return $ liftM fst $
  --where
  --  rpaths = reverse paths





























