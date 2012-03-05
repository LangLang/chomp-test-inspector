{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Network.Wai (Response, Application, responseLBS, requestBody)
import Network.HTTP.Types (status200, status400)
import Network.Wai.Handler.Warp (run)
import Network.WebSockets(WebSockets, TextProtocol, sendTextData)
import qualified Data.Text as T
--import qualified Data.ByteString.Lazy as LB
import Text.Hamlet (shamlet)
import Text.Blaze.Renderer.Utf8 (renderHtml)
import System.INotify (initINotify, killINotify, addWatch, removeWatch, EventVariety(..), Event)


main :: IO ()
main = do
  inotify <- initINotify
  addWatch inotify [Modify] "tests/test00.source" sourceFileChanged
  run 8080 app
  killINotify inotify

sourceFileChanged :: Event -> IO ()
sourceFileChanged e = do
  return ()

webSocketApp :: TextProtocol p => WebSockets p ()
webSocketApp = sendTextData (T.pack "Test websocket response")

app :: Application
app req = do
  --body <- requestBody req
  return $ responseLBS
    status200
    [("Content-Type", "text/html")]
    $ renderHtml [shamlet|
        $doctype 5
        <html>
          <head>
            <title>#{pageTitle} &mdash; #{pageSubTitle}
            <link>
          <body>
            <h1 .page-title>#{pageTitle}
            <p>Reactive tests
            <textarea>#{test1Src}
            <textarea placeholder="Result...">
            <footer>^{copyright}
      |]
      where
        pageTitle = "Chomp (A brave new LangLang compiler)" :: String
        pageSubTitle = "Test Inspector" :: String
        test1Src = "" :: String
        copyright = [shamlet|<p>Copyright (C) Rehno Lindeque.|]
