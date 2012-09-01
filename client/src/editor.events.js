  var Editor = Editor || {};
  (function(){
    Editor.bindEvents = function() {
      $('#editors').on('keyup', '.supersimple-editor-input', function(e){
        var 
          code = e.charCode? e.charCode : e.keyCode,
          $editorInput = $(e.target),
          $editor = $editorInput.parent().parent(),
          filename = $editor.attr("data-filename"),
          isResult = $editorInput.hasClass("editor-result");
        if (filename == null)
          console.error("No filename attribute found for editor.")
        if (isResult)
          filename += ".output";

        // Ignore pageup/pagedown/home/end
        if (code >= 33 && code <= 36)
          return;
        // Ignore arrow keys
        if (code >= 37 && code <= 40)
          return;
        // Ignore shift/control/alt
        if (code == 0 || (code >= 16 && code <= 18))
          return;

        Editor.highlight(filename);
        
        // Generate an operation for changes to the buffer (since before this key event)
        // TODO: Find a fast way of doing this (e.g. by marking html spans as dirty)
        
        // Send the operations to the server to be persisted
        // TODO
        //MessageService.send();
      });
    };
  })();