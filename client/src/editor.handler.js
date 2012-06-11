  var Editor = Editor || {};
  (function(html){
    var 
      createEditor = function(fileName) {
        return html.div({class: "editor"},
          html.div({class: "editor-source"},
            html.input({class: "editor-source-filename", value: fileName}),
            html.editor()
          ),
          html.div({class: "editor-result"},
            html.input({class: "editor-result-filename", readonly: "readonly", value: fileName + ".??? (TODO)"}),
            html.editor()
          ),
          html.div({style: "clear:both"})
        );
      },
      enableEditor = function() {
        $(".disable-overlay").hide();
      },
      disableEditor = function() {
        $(".disable-overlay").show();
      };

    Editor.handler = adt.recursive(adt({
      Connected: enableEditor,
      RestoredRootDirectory: enableEditor,
      MovedOutRootDirectory: disableEditor,
      DeleteRootDirectory: disableEditor,
      UnmountedRootDirectory: disableEditor,
      ReloadFiles: function(storageEvent, files) { 
        var i;
        $('#editors').html('' );
        for (i = 0; i < files.length; ++i)
          $('#editors').append(createEditor(files[i]));
      },
      LoadFile: function(storageEvent, file) { 
        $('#editors')
          .find(".editor-source-filename[value='" + file + "']")
          .closest('.editor')
          .remove();
        $('#editors').append(createEditor(file));
      },
      UnloadFile: function(storageEvent, file) { 
        $('#editors')
          .find(".editor-source-filename[value='" + file + "']")
          .closest('.editor')
          .remove();
      }
    }));
  })(adt({ editor: supersimple.editor.html }, html.evalCons));
