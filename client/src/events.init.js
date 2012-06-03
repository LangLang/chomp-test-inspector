  var Init = {
    messageService: function(){ 
      state.ws = WebSocketService.create('/');
      MessageService.attach(state.ws, [Log.handler, Editor.handler, StatusBar.handler]);
    }
  };

  $(document).ready(function () {
    Init.messageService();
  });
