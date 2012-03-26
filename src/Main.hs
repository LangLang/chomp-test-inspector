{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

-- Standard modules
import System.IO (stdout, hFlush)
import Data.Text hiding (map)
import Data.Text.IO (putStrLn, hPutStrLn)
import Prelude hiding (putStrLn)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets (interceptWith)
import Network.WebSockets(defaultWebSocketsOptions)
import System.INotify (initINotify, killINotify, addWatch, removeWatch, EventVariety(..), Event(..))
--import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import Control.Concurrent.STM (TVar, atomically)
import Data.STM.TList (TList)
import qualified Data.STM.TList as TList
import Control.Exception (try)
import Control.Monad.Trans (liftIO)
import qualified System.Directory as Dir
import Data.String.Utils (endswith)

-- Application modules
import WebApp
import WebsocketApp
import qualified STM.FileStore as STM (FileStore)
import qualified STM.FileStore as STM.FileStore

-- Main application entry point
main :: IO ()
main = do
  --TODO: initialFiles <- try $ getDirectoryContents watchPath
  initialFiles <- Dir.getDirectoryContents watchPath
  fileStore <- atomically $ STM.FileStore.fromPaths initialFiles
  --fileStore <- newMVar initialFiles
  inotify <- initINotify
  addWatch inotify [Modify] watchPath $ sourceFileChanged fileStore
  Warp.runSettings (webAppSettings fileStore) webApp
  killINotify inotify
  where
    watchPath = "tests" :: FilePath
    isDots f = not $ (endswith "/." f) || (endswith "/.." f)
    sourceFileChanged :: STM.FileStore -> Event -> IO ()
    sourceFileChanged fileStore e = do
      case e of
        Modified False p -> putStrLn $ (fromMaybeFilePath p) `append` " was modified."
        MovedOut False p c -> do
          putStrLn $ "'" `append` pack p `append` "' was moved out."
          --readMVar fileStore
          do
            --n <- atomically $ TList.start fileStore
            return ()
        MovedIn False p c -> putStrLn $ "'" `append` pack p `append` "' was moved in." --TODO: use the cookie to check whether the file was actually renamed
        MovedSelf _ -> putStrLn "The watched path was moved and hence no longer exists."
        Created True p -> putStrLn $ "'" `append` pack p `append` "' was created."
        Deleted True p -> putStrLn $ "'" `append` pack p `append` "' was deleted."
        DeletedSelf -> putStrLn "The watched path was moved and hence no longer exists."
        Unmounted -> putStrLn "The watched path was unmounted and hence no longer exists."
        QOverflow -> putStrLn "TODO: The queue overflowed, resend all the files."
        _ -> return ()
      where
        fromMaybeFilePath :: Maybe FilePath -> Text
        fromMaybeFilePath = maybe "Unknown file" $ \filename -> "'" `append` pack filename `append` "'"

-- Set up the web application with the websocket app and a thread-safe file store
webAppSettings :: STM.FileStore -> Warp.Settings
webAppSettings fileStore = Warp.defaultSettings
  { Warp.settingsPort = 8080
  , Warp.settingsIntercept = interceptWith defaultWebSocketsOptions $ websocketApp fileStore
  }
