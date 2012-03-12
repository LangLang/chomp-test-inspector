{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Network.Wai (Response, Application, responseLBS, requestBody, pathInfo)
import Network.HTTP.Types (status200, status400)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Application.Static -- TODO, refine import
import Network.WebSockets(WebSockets, TextProtocol, sendTextData)
import qualified Data.Text as T
--import qualified Data.ByteString.Lazy as LB
import System.INotify (initINotify, killINotify, addWatch, removeWatch, EventVariety(..), Event)
import TestInspectorPage

--import Control.Monad
import Control.Monad.Trans

import System.IO (stdout, hFlush)
import Data.Text hiding (map)
import Data.Text.IO (putStrLn, hPutStrLn)
import Prelude hiding (putStrLn)


main :: IO ()
main = do
  inotify <- initINotify
  addWatch inotify [Modify] "tests/test00.source" sourceFileChanged
  run 8080 app
  killINotify inotify
  where
    app req =
      let p = pathInfo req :: [Text] in
      case p of
        _:_ -> do
          liftIO $ putStrLn $ intercalate "/" p
          staticInspectorApp req
        _          ->
          inspectorApp req

  --liftIO $ putStrLn $ intercalate "/"
  --app req

sourceFileChanged :: Event -> IO ()
sourceFileChanged e = do
  return ()

webSocketInspectorApp :: TextProtocol p => WebSockets p ()
webSocketInspectorApp = sendTextData (T.pack "Test websocket response")

staticInspectorApp :: Application
staticInspectorApp = staticApp defaultWebAppSettings
{-
staticInspectorApp req = do
  -- responseFile
  --liftIO $ sequence_ $ map putStrLn $ pathInfo req
  case p of
    "static":_ -> do
      liftIO $ putStrLn $ intercalate "/" p
      inspectorApp req
    _          ->
      inspectorApp req
  --liftIO $ putStrLn $ intercalate "/"
  --app req
  where
    p = pathInfo req :: [Text]
-}

inspectorApp :: Application
inspectorApp req = do
  --body <- requestBody req
  return $ responseLBS
    status200
    [("Content-Type", "text/html")]
    $ pageHtml

