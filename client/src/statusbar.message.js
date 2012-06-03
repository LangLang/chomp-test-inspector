  var StatusBar = StatusBar || {};
  (function(html){
    StatusBar.info = function(message) {
      $('#page-statusbar').html(html.span({class: "status-info"}, html.em(message)));
    };
  })(html.evalCons);
