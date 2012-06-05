{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-} -- to control production versus debug
 
module TestInspectorPage (pageHtml) where

-- Standard modules
import qualified Data.Text.Lazy as LT (concat)
import qualified Data.ByteString.Lazy as L (ByteString)
import Text.Blaze (preEscapedLazyText)
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Text.Hamlet (shamlet)
import Text.Julius (JavascriptUrl, renderJavascriptUrl)
import Text.Cassius (CssUrl, renderCssUrl)
import Text.Shakespeare.Text (renderTextUrl)
#if PRODUCTION
import Text.Julius (jsFile)
--import Text.Lucius (luciusFile)
import Text.Cassius (cassiusFile)
import Text.Shakespeare.Text (textFile)
#else
import Text.Julius (jsFileReload)
--import Text.Lucius (luciusFileReload)
import Text.Cassius (cassiusFileReload)
import Text.Shakespeare.Text (textFileReload)
#endif

-- Html page
pageHtml :: L.ByteString
pageHtml = renderHtml [shamlet|
    $doctype 5
    <html>
      <head>
        <title>#{pageTitle} &mdash; #{pageSubTitle}
        <link rel=stylesheet href=normalize/normalize.css>
        <script src=/socket.io/socket.io.js>
        <script src=/jquery/jquery.min.js>
        <script src=/adt/adt.js>
        <script src=/adt/adt-html.js>
        <script src=/supersimple/supersimple-editor.js>
        <style type=text/css>
          ^{css}
        <script>
          ^{js}
      <body>
        <h1 #page-title>#{pageTitle}
        <h2 #page-sub-title>A brave new <a href=http://langlang.org>LangLang</a> compiler
        <div #page-background>
          <div #page-background-results>
          <div #editors>
        <div .disable-overlay>
        <footer #page-statusbar>
  |]
  where
    dummyRouter _ _ = undefined
    js = preEscapedLazyText $ LT.concat $ map (renderJavascriptUrl dummyRouter) jsFiles
    css = preEscapedLazyText $ LT.concat $ (map (renderCssUrl dummyRouter) $ cassiusFiles)
      ++ map (renderTextUrl dummyRouter) cssFiles
    pageTitle = "Chomp" :: String 
    pageSubTitle = "A brave new LangLang compiler" :: String

jsFiles :: [JavascriptUrl ()]
#if PRODUCTION
jsFiles = [
    $(jsFile "client/src/header.js"),
    $(jsFile "client/src/core.js"),
    $(jsFile "client/src/state.js"),
    $(jsFile "client/src/editor.highlight.js"),
    $(jsFile "client/src/statusbar.message.js"),
    $(jsFile "client/src/websocket.service.js"),
    $(jsFile "client/src/message.js"),
    $(jsFile "client/src/message.service.js"),
    $(jsFile "client/src/log.handler.js"),
    $(jsFile "client/src/editor.handler.js"),
    $(jsFile "client/src/statusbar.handler.js"),
    $(jsFile "client/src/events.init.js"),
    $(jsFile "client/src/footer.js")
  ]
#else
jsFiles = [
    $(jsFileReload "client/src/header.js"),
    $(jsFileReload "client/src/core.js"),
    $(jsFileReload "client/src/state.js"),
    $(jsFileReload "client/src/editor.highlight.js"),
    $(jsFileReload "client/src/statusbar.message.js"),
    $(jsFileReload "client/src/websocket.service.js"),
    $(jsFileReload "client/src/message.js"),
    $(jsFileReload "client/src/message.service.js"),
    $(jsFileReload "client/src/log.handler.js"),
    $(jsFileReload "client/src/editor.handler.js"),
    $(jsFileReload "client/src/statusbar.handler.js"),
    $(jsFileReload "client/src/events.init.js"),
    $(jsFileReload "client/src/footer.js")
  ]
#endif

cassiusFiles :: [CssUrl ()]
#if PRODUCTION
cassiusFiles = [
    $(cassiusFile "client/style/main.cassius")
  ]
#else
cassiusFiles = [
    $(cassiusFileReload "client/style/main.cassius")
  ]
#endif

-- Neither lucius nor cassius support CSS3 animation @keyframes properly, so text files are needed for this
--cssFiles :: [TextUrl ()]
#if PRODUCTION
cssFiles = [
    $(textFile "client/style/effect.css"),
    $(textFile "client/style/animations.css")
  ]
#else
cssFiles = [
    $(textFileReload "client/style/effect.css"),
    $(textFileReload "client/style/animations.css")
  ]
#endif