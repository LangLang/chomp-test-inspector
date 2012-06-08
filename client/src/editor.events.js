  var Editor = Editor || {};
  (function(){
    Editor.bindEvents = function() {
      $('#editors').on('keyup', '.supersimple-editor-input', function(e){
        Editor.highlight(e.target);
      });
    };
  })();