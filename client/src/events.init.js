  var Init = {
    messageService: function() { 
      state.ws = WebSocketService.create('/');
      MessageService.attach(state.ws, [Log.handler, Editor.handler, StatusBar.handler]);
    },
    statusMessage: function() {
      StatusBar.info("Turtles...");
      setTimeout(function(){ StatusBar.info("...all the way down") }, 2100);
    }
  };

  $(document).ready(function() {
    Init.messageService();
    Init.statusMessage();
  });
