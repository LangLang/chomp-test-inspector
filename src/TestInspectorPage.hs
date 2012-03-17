{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module TestInspectorPage (pageHtml) where

import Text.Hamlet (shamlet)
import Text.Blaze.Renderer.Utf8 (renderHtml)
--import Text.Julius (renderJavascript, js)
import Text.Shakespeare.Text (st)
import Text.Blaze (preEscapedText)

-- Html page
pageHtml = renderHtml [shamlet|
    $doctype 5
    <html>
      <head>
        <title>#{pageTitle} &mdash; #{pageSubTitle}
        <link>
        <script src=/socket.io/socket.io.js>
        <script src=/jquery/jquery.js>
        <script>
          /*var socket = io.connect('http://localhost');
          socket.on('news', function (data) {
            console.log(data);
            socket.emit('my other event', { my: 'data' });
          });*/
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
    js = preEscapedText pageJs
    pageTitle = "Chomp (A brave new LangLang compiler)" :: String
    pageSubTitle = "Test Inspector" :: String
    test1Src = "" :: String
    copyright = "Copyright (C) Rehno Lindeque."

-- Embedded javascript in the html page
--pageJs = renderJavascript $ [js|
pageJs = [st|
    createWebSocket = function(path) {
      var host = window.location.hostname;
      if(host == '') host = 'localhost';
      var uri = 'ws://' + host + ':8080' + path;

      var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
      return new Socket(uri);
    };

    $(document).ready(function () {
      var ws = createWebSocket('/');

      ws.onopen = function() {
        ws.send('Message from client');
      };

      ws.onmessage = function(event) {
        console.log(event);
      };
    });
  |]
