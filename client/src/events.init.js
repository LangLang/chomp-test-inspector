  var Init = {
    messageService: function(){ 
      state.ws = WebSocketService.create('/');
      MessageService.attach(state.ws, [Log.handler, Editor.handler]);
    }
  };

  $(document).ready(function () {
    Init.messageService();
  });
