  var Editor = Editor || {};
  (function(html){
    Editor.handler = adt({
      ReloadFiles: function(files) { 
        var i;
        $('#editors').html();
        for (i = 0; i < files.length; ++i)
          $('#editors').append(
            html.div({class: "editor"},
              html.div({class: "editor-source"},
                html.input({class: "editor-source-filename", value: files[i]}),
                html.editor()
              ),
              html.div({class: "editor-result"},
                html.input({class: "editor-result-filename", readonly: "readonly", value: files[i] + ".??? (TODO)"}),
                html.editor()
              ),
              html.div({style: "clear:both"})
            )
          );
      }
    });
  })(adt({ editor: supersimple.editor.html }, html.evalCons));
