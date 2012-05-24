{-# LANGUAGE  CPP #-}
module Main (main) where

-- Standard modules
import Data.Text (pack)
import Data.Text.IO (hPutStrLn)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets (interceptWith)
import Network.WebSockets(defaultWebSocketsOptions)
import System.IO (stderr)
import System.IO.Error (try, ioeGetErrorType, IOErrorType)
#ifdef __GLASGOW_HASKELL__
import qualified GHC.IO.Exception as Exception
#endif

-- Application modules
import WebApp
import WebsocketApp
import qualified STM.FileStore as STM (FileStore)
import qualified STM.FileStore as STM.FileStore
import FileObserver

-- Application entry point
main :: IO ()
main = do
  -- Instantiate shared resources
  fileStore <- STM.FileStore.newIO
  --serverMessages <- STM.Messages.newIO -- TODO
  -- Run asynchronous observers
  eitherExceptionOrId <- try $ forkFileObserver watchPath fileStore
  case eitherExceptionOrId of
    Left e ->
      case generateErrorMessage $ ioeGetErrorType e of 
        Just message -> hPutStrLn stderr $ pack message
        Nothing -> ioError e
    Right observerId -> do 
      -- Run the front controllers
      Warp.runSettings (webAppSettings fileStore) webApp
      -- Stop the asynchronous observers
      killFileObserver observerId  
  where
    watchPath = "tests" :: FilePath
     
    -- Generate an error message 
    generateErrorMessage :: IOErrorType -> Maybe String
#ifdef __GLASGOW_HASKELL__
    generateErrorMessage errorType = case errorType of
      -- GHC only:
      Exception.NoSuchThing -> Just $ "The path supplied `" ++ watchPath ++ "` does not exist."
      Exception.PermissionDenied -> Just $ "Permission to read the the path `" ++ watchPath ++ "` was denied."      
      Exception.InvalidArgument -> Just $ "The path supplied `" ++ watchPath ++ "` is not a valid directory name."
      Exception.InappropriateType -> Just $ "The path supplied `" ++ watchPath ++ "` refers to a non-directory object."
      _ -> Nothing
#else
    generateErrorMessage errorType = Nothing
#endif

-- Set up the web application (front controller) with the websocket application (front controller)
-- and shared resources (the file store and incoming message queues)
webAppSettings :: STM.FileStore -> Warp.Settings
webAppSettings fileStore = Warp.defaultSettings
  { Warp.settingsPort = 8080
  , Warp.settingsIntercept = interceptWith defaultWebSocketsOptions $ websocketApp fileStore
  }
  
  
  
  
  
  
  
  
  
