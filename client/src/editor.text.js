  var Editor = Editor || {};
  (function(){
    Editor.read = function(file) {
      return $(Editor.DOM.getInput(file)).text();
    };
    Editor.update = function(file, text) {
      $(Editor.DOM.getInput(file)).text(text);
    };
    Editor.clear = function(file) {
      $(Editor.DOM.getInput(file)).text("");
    };
  })();