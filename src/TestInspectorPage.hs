{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-} -- to control production versus debug

module TestInspectorPage (pageHtml) where

import Text.Hamlet (shamlet)
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Text.Julius (JavascriptUrl, renderJavascriptUrl, jsFile, jsFileReload)
import Text.Blaze (preEscapedLazyText)
import qualified Data.Text.Lazy as LT (concat)

-- Html page
pageHtml = renderHtml [shamlet|
    $doctype 5
    <html>
      <head>
        <title>#{pageTitle} &mdash; #{pageSubTitle}
        <link>
        <script src=/socket.io/socket.io.js>
        <script src=/jquery/jquery.min.js>
        <script src=/adt/adt.js>
        <script>
          ^{js}
      <body>
        <h1 .page-title>#{pageTitle}
        <p>Reactive tests
        <textarea>#{test1Src}
        <textarea placeholder="Result...">
        <footer>^{copyright}
  |]
  where
    js = preEscapedLazyText $ LT.concat $ map (renderJavascriptUrl (\_ _ -> undefined)) jsFiles
    pageTitle = "Chomp (A brave new LangLang compiler)" :: String
    pageSubTitle = "Test Inspector" :: String
    test1Src = "" :: String
    copyright = "Copyright (C) Rehno Lindeque."

#if PRODUCTION
jsFiles = [
    $(jsFile "message.js"),
    $(jsFile "websocket-service.js")
  ]
#else
jsFiles = [
    $(jsFileReload "message.js"),
    $(jsFileReload "websocket-service.js")
  ]
#endif
