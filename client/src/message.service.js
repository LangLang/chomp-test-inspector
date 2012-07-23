  var MessageService = MessageService || {};
  (function(){
    var _publish = function(handlers, message) {
      for (var i = 0; i < handlers.length; ++i)
        handlers[i](message);
    };
    MessageService.subscribe = function(ws, handlers) {
      if (ws == null)
        return;
      ws.onopen = function() {
        // TODO: do something meaningful here
      };
      ws.onmessage = function(event) {
        console.log("Message received...", event);
        _publish(handlers, adt.deserialize(event.data));
      };
      ws.onclose = function() {
        console.log("Connection closed...");
        _publish(handlers, Message.ConnectionClosed());
      };
    };
    // Send a message to the server
    MessageService.send = function(ws, message) {
      ws.send(adt.serialize(message));
    };
  })();
