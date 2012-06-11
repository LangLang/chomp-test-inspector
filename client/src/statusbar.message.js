  var StatusBar = StatusBar || {};
  (function(html){
    StatusBar.info = function() {
      $('#page-statusbar').html(html.span({class: "status-info"}, html.em.apply(null, arguments)));
    };
    StatusBar.important = function() {
      $('#page-statusbar').html(html.span({class: "status-important"}, html.strong.apply(null, arguments)));
    };
  })(html.evalCons);
