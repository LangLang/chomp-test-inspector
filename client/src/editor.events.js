  var Editor = Editor || {};
  (function(){
    Editor.bindEvents = function() {
      $('#editors').on('keyup', '.supersimple-editor-input', function(e){
        var code = e.charCode? e.charCode : e.keyCode;
        // Ignore arrow keys
        if (code >= 37 && code <= 40)
          return;
        // Ignore shift/control/alt
        if (code == 0 || (code >= 16 && code <= 18))
          return;
        Editor.highlight(e.target);
      });
    };
  })();