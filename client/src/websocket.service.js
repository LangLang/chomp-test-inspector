  // Websocket service
  var WebSocketService = {
    // Create a websocket
    create: function(path) {
      var host = window.location.hostname;
      if(host == '') host = 'localhost';
      var uri = 'ws://' + host + ':8080' + path;
      var Socket = 'MozWebSocket' in window ? MozWebSocket : WebSocket;
      var ws = new Socket(uri);
      return ws;
    }
  };
