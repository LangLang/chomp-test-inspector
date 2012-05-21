  var Init = {
    messageService: function(){ 
      state.ws = WebSocketService.create('/');
      MessageService.attach(state.ws);
    }
  };

  $(document).ready(function () {
    Init.messageService();
  });
