  var StatusBar = StatusBar || {};
  (function(html){
    StatusBar.info = function(message) {
      $('#page-statusbar').html(html.span({class: "status-info"}, html.em(message)));
    };
    StatusBar.important = function(message) {
      $('#page-statusbar').html(html.span({class: "status-important"}, html.strong(message)));
    };
  })(html.evalCons);
