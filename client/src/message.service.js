  var MessageService = {
    // Attach to a websocket
    attach: function (ws, handlers) {
      if (ws == null)
        return;
      ws.onopen = function() {
        //sendMessage(ws, "Message from client");
        // TODO: do something meaningful here
      };
      ws.onmessage = function(event) {
        console.log("Message received...");
        console.log("...event: ", event);
        var message = adt.deserialize(event.data);
        for (var i = 0; i < handlers.length; ++i)
          handlers[i](message);
      };
    },
    // Send a message to the server
    sendMessage: function(ws, message) {
      ws.send(adt.serialize(message));
    }
  };
