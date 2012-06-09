{-# LANGUAGE OverloadedStrings #-}
module WebApp (webApp) where

-- Standard modules
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.Trans (liftIO)
import Network.HTTP.Types (status200, status400)
import qualified Network.Wai as W
import qualified Network.Wai.Application.Static as WAS

-- Application
import TestInspectorPage

-- Web application responsible for serving the html page and all static files to the browser
webApp req =
  let p = W.pathInfo req :: [T.Text] in
  case p of
    _:_ -> do
      liftIO $ T.putStrLn $ T.intercalate (T.pack "/") p
      staticApp req
    _          ->
      pageApp req

-- Static file server
staticApp :: W.Application
staticApp = WAS.staticApp WAS.defaultWebAppSettings

-- Serves the main web page (html and javascript)
pageApp :: W.Application
pageApp req = do
  --body <- requestBody req
  return $ W.responseLBS
    status200
    [("Content-Type", "text/html;charset=utf-8")]
    $ pageHtml

