{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-} -- to control production versus debug

module TestInspectorPage (pageHtml) where

import Text.Hamlet (shamlet)
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Text.Julius (renderJavascriptUrl, jsFile, jsFileReload)
import Text.Cassius (renderCssUrl, cassiusFile, cassiusFileReload)
import Text.Blaze (preEscapedLazyText)
import qualified Data.Text.Lazy as LT (concat)

-- Html page
pageHtml = renderHtml [shamlet|
    $doctype 5
    <html>
      <head>
        <title>#{pageTitle} &mdash; #{pageSubTitle}
        <link rel=stylesheet href=normalize/normalize.css>
        <script src=/socket.io/socket.io.js>
        <script src=/jquery/jquery.min.js>
        <script src=/adt/adt.js>
        <style type=text/css>
          ^{css}
        <script>
          ^{js}
      <body>
        <h1 .page-title>#{pageTitle}
        <h2 .page-sub-title>A brave new <a href=http://langlang.org>LangLang</a> compiler
        <div .page-background>
          <div .page-background-results>
          <div #editors>
        <footer>Copyright &copy; <a href=http://rehno.lindeque.name/>Rehno Lindeque</a>.
  |]
  where
    dummyRouter _ _ = undefined
    js = preEscapedLazyText $ LT.concat $ map (renderJavascriptUrl dummyRouter) jsFiles
    css = preEscapedLazyText $ LT.concat $ map (renderCssUrl dummyRouter) cassiusFiles
    pageTitle = "Chomp" :: String
    pageSubTitle = "" :: String
    test1Src = "" :: String

#if PRODUCTION
jsFiles = [
    $(jsFile "client/src/message.js"),
    $(jsFile "client/src/websocket-service.js")
  ]
#else
jsFiles = [
    $(jsFileReload "client/src/message.js"),
    $(jsFileReload "client/src/websocket-service.js")
  ]
#endif

#if PRODUCTION
cassiusFiles = [
    $(cassiusFile "client/style/main.cassius")
  ]
#else
cassiusFiles = [
    $(cassiusFileReload "client/style/main.cassius")
  ]
#endif

