  var Editor = Editor || {};
  (function(){
    var 
      isResultFile = function(file) { return /\.output$/.test(file); };
    Editor.read = function(file) {
      var
        isResult = isResultFile(file),
        baseFilename = isResult? file.slice(0, file.length - ".output".length) : file,
        $editors = $("#editors"),
        $editor = $editors.find(".editor[data-filename='" + baseFilename + "']"),
        selector = isResult? ".editor-result" : ".editor-source",
        $editorInput = $editor.find(selector).find(".supersimple-editor-input");
      if ($editor.length !== 1 || $editorInput.length !== 1)
        throw "No editor found for `" + String(file) + "`.";
      return $editorInput.text();
    };
    Editor.update = function(file, text) {
      var
        isResult = isResultFile(file),
        baseFilename = isResult? file.slice(0, file.length - ".output".length) : file,
        $editors = $("#editors"),
        $editor = $editors.find(".editor[data-filename='" + baseFilename + "']"),
        selector = isResult? ".editor-result" : ".editor-source",
        $editorInput = $editor.find(selector).find(".supersimple-editor-input");
      if ($editor.length !== 1 || $editorInput.length !== 1)
        throw "No editor found for `" + String(file) + "`.";
      $editorInput.text(text);
      // TODO: highlight... and get changes after highlight
    };
    Editor.clear = function(file) {
      var
        isResult = isResultFile(file),
        baseFilename = isResult? file.slice(0, file.length - ".output".length) : file,
        $editors = $("#editors"),
        $editor = $editors.find(".editor[data-filename='" + baseFilename + "']"),
        selector = isResult? ".editor-result" : ".editor-source",
        $editorInput = $editor.find(selector).find(".supersimple-editor-input");
      if ($editor.length !== 1 || $editorInput.length !== 1)
        throw "No editor found for `" + String(file) + "`.";
      $editorInput.text("");
    };
  })();