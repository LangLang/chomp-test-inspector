// Websocket service
(function(){
  var
    Message = adt('Acknowledge','Notify','ReloadFiles','PatchFile', 'ParseError'),
    evalMessage = adt({
      Acknowledge: function() { console.log("...previous message acknowledged"); },
      Notify: function(notification) { console.log("...notification:", notification); },
      ReloadFiles: function(files) { 
        console.log("...reload files: ", files); 
      },
      PatchFile: function(filePath, patch) {
        console.log("...apply patch to file: ", file);
        console.log("...patch: ", patch);
      },
      ParseError: function(message) { console.error("...previous message this client sent could not be parsed: \n", message); },
      _: function() { console.error("...(error) unknown message type `" + this._key + "`" ); }
    }),

    sendMessage = function(ws, message) {
      ws.send(adt.serialize(message));
    },

    createWebSocket = function(path) {
      var host = window.location.hostname;
      if(host == '') host = 'localhost';
      var uri = 'ws://' + host + ':8080' + path;
      var Socket = 'MozWebSocket' in window ? MozWebSocket : WebSocket;
      return new Socket(uri);
    };

  $(document).ready(function () {
    var ws = createWebSocket('/');

    ws.onopen = function() {
      //sendMessage(ws, "Message from client");
      // TODO: do something meaningful here
    };

    ws.onmessage = function(event) {
      console.log("Message received...");
      console.log("...event: ", event);
      evalMessage(adt.deserialize(event.data));
    };
  });
})();