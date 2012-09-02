  var Editor = Editor || {};
  (function(){
    Editor.bindEvents = function() {
      // Handle all printable characters
      $('#editors').on('keypress', '.supersimple-editor-input', function(e){
        var 
          charCode = e.which,
          textContent = e.target.innerText ? e.target.innerText : e.target.textContent,
          $editorInput = $(e.target),
          $editor = $editorInput.parent().parent(),
          filename = $editor.attr("data-filename"),
          isResult = $editorInput.hasClass("editor-result"),
          caretOffset,
          otClient,
          otOperations;
        console.log("keypress", charCode, e);

        // Ignore non-printable characters
        if (charCode == 0)
          return;

        // TODO: Handle the case where there's already a selection (to be replaced) in the editor

        // Generate an operation for due changes to the buffer
        caretOffset = Editor.getCaretOffset(e.target);
        if (caretOffset == null) {
          console.error("Could not determine the caret offset during a keypress event.");
          return;
        }

        otClient = Editor.getOTClient(filename);
        otOperations = otClient.createOperation();
        otOperations.retain(caretOffset);

        switch (charCode) {
          case 13: // Return key '\r'
            otOperations.insert('\n');
            break;
          default:
            otOperations.insert(String.fromCharCode(charCode));
        }
        otOperations.retain(textContent.length - caretOffset);
        console.log("keypress", otOperations);

        // Apply operations to the client
        otClient.applyClient(otOperations);
      });

      // Handle all non-printable characters
      $('#editors').on('keydown', '.supersimple-editor-input', function(e){
        var 
          keyCode = e.which,
          textContent = e.target.innerText ? e.target.innerText : e.target.textContent,
          $editorInput = $(e.target),
          $editor = $editorInput.parent().parent(),
          filename = $editor.attr("data-filename"),
          isResult = $editorInput.hasClass("editor-result"),
          caretOffset,
          otClient,
          otOperations,
          remainingCharacters;
        if (filename == null)
          console.error("No filename attribute found for editor.")
        if (isResult)
          filename += ".output";

        // Ignore pageup/pagedown/home/end
        if (keyCode >= 33 && keyCode <= 36)
          return;
        // Ignore arrow keys
        if (keyCode >= 37 && keyCode <= 40)
          return;
        // Ignore shift/control/alt
        if (keyCode == 0 || (keyCode >= 16 && keyCode <= 18))
          return;
        // Ignore Esc
        if (keyCode == 27)
          return;

        // TODO: Handle the case where there's already a selection (to be replaced) in the editor

        // Generate an operation for changes to the buffer (since before this key event)
        caretOffset = Editor.getCaretOffset(e.target);
        if (caretOffset == null) {
          console.error("Could not determine the caret offset during a keyup event.");
          return;
        }

        switch (keyCode) {
          case 8: // Backspace
            if (caretOffset == 0) return;
          //case 45: // Insert
          case 46: // Delete
            if (caretOffset == textContent.length) return;
          //case 86: // V (ctrl+v = paste)
          //case 88: // V (ctrl+x = cut)
            break;
          default: 
            // Unknown keycode - do nothing
            return;
        };

        otClient = Editor.getOTClient(filename);
        otOperations = otClient.createOperation();
        otOperations.retain(caretOffset);
        remainingCharacters = textContent.length - caretOffset;

        switch (keyCode) {
          case 8: // Backspace
            otOperations.backspace(1);
            break;
          //case 45: // Insert
          case 46: // Delete
            otOperations.delete(1);
            remainingCharacters -= 1;
            break;
          //case 86: // V (ctrl+v = paste)
          //case 88: // V (ctrl+x = cut)
        };
        otOperations.retain(remainingCharacters);
        console.log("keydown", otOperations);

        // Apply operations to the client
        otClient.applyClient(otOperations);

        // Perform the highlight operation to reformat the code
        Editor.highlight(filename);
      });
    };
  })();