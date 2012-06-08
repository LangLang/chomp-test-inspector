  $(document).ready(function() {
    var Init = {
      messageService: function() { 
        state.ws = WebSocketService.create('/');
        MessageService.subscribe(state.ws, [Log.handler, Editor.handler, StatusBar.handler]);
      },
      statusMessage: function() {
        StatusBar.info("Turtles...");
        setTimeout(function(){ StatusBar.info("...all the way down") }, 2100);
      },
      editor: function() {
        Editor.bindEvents();
      }
    };

    Init.messageService();
    Init.statusMessage();
    Init.editor();
  });
