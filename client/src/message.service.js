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
        console.log("Message received...\n\t", event);
        _publish(handlers, adt.deserialize(event.data));
      };
      ws.onclose = function() {
        console.log("Connection closed...");
        _publish(handlers, Message.ConnectionClosed());
      };
    };
    // Send a message to the server
    MessageService.send = function(ws, message) {
      var str = adt.serialize(message);
      ws.send(str);
      console.log("Message sent...\n\t", {ws: ws, message: str});
    };
  })();
