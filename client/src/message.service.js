  var MessageService = {
    // Attach to a websocket
    attach: function (ws) {
      if (ws == null)
        return;
      ws.onopen = function() {
        //sendMessage(ws, "Message from client");
        // TODO: do something meaningful here
      };
      ws.onmessage = function(event) {
        console.log("Message received...");
        console.log("...event: ", event);
        evalMessage(adt.deserialize(event.data));
      };
    }
  };
