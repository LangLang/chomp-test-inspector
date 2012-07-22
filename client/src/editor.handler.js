  var Editor = Editor || {};
  (function(html){
    var 
      createDropDownLog = function() {
        return html.div({class: "editor-log editor-log-empty"}, 
          html.div({class: "editor-log-dropdown"}),
          html.div({class: "editor-log-lines"})
        );
      },
      createEditor = function(fileName) {
        return html.div({class: "editor"},
          html.div({class: "editor-source"},
            html.input({class: "editor-source-filename", value: fileName}),
            html.editor()
          ),
          html.div({class: "editor-result"},
            html.input({class: "editor-result-filename", readonly: "readonly", value: fileName + ".??? (TODO)"}),
            html.editor(),
            createDropDownLog()
          ),
          html.div({style: "clear:both"})
        );
      },
      enableEditor = function() {
        $(".disable-overlay").hide();
      },
      disableEditor = function() {
        $(".disable-overlay").show();
      },
      showExitCode = adt({
        ExitSuccess: "Success",
        ExitFailure: function(code) { return "Failure: " + String(code); }
      }),
      createLogLine = adt({
        LogStart: function() { return html.div("Executing..."); },
        LogInfo: function(message) { return html.div({class: "editor-log-info"}, message); },
        LogError: function(message) { return html.div({class: "editor-log-error"}, message); },
        LogEnd: function(exitCode) { return html.div({class: "editor-log-end"}, "...Done (" + showExitCode(exitCode) + ")" ); }
      });

    Editor.handler = adt.recursive(adt({
      Connected: enableEditor,
      RestoredRootDirectory: enableEditor,
      MovedOutRootDirectory: disableEditor,
      DeleteRootDirectory: disableEditor,
      UnmountedRootDirectory: disableEditor,
      ProcessMessage: function(file, message) {
        var
          logLine = createLogLine(message),
          $editorLog = $("#editors")
            .find(".editor-source-filename[value='" + file + "']")
            .closest(".editor")
            .find(".editor-log")
            .removeClass('editor-log-empty'),
          $lines = $editorLog.find(".editor-log-lines");
        adt({ LogStart: function() { $lines.empty(); } })(message);
        $lines.append(logLine);
      },
      ReloadFiles: function(storageEvent, files) { 
        var i;
        $("#editors").html("");
        for (i = 0; i < files.length; ++i)
          $("#editors").append(createEditor(files[i]));
      },
      LoadFile: function(storageEvent, file) { 
        $("#editors")
          .find(".editor-source-filename[value='" + file + "']")
          .closest(".editor")
          .remove();
        $("#editors").append(createEditor(file));
      },
      LoadFileContents: function(file, maybeContents) {
        var $editor = $("#editors")
          .find(".editor-source-filename[value='" + file + "']")
          .closest(".editor-source")
          .find(".supersimple-editor-input");
        if ($editor.length !== 1)
          return;        
        adt({
          Just: function(contents) { 
            $editor
              .text(contents)
              .attr('contenteditable', 'true');
            Editor.highlight($editor.get(0));
          },
          Nothing: function() {
            $editor
              .text("")
              .attr('contenteditable', 'false');
          }
        })(maybeContents);
      },
      UnloadFile: function(storageEvent, file) { 
        $("#editors")
          .find(".editor-source-filename[value='" + file + "']")
          .closest(".editor")
          .remove();
      }
    }));
  })(adt({ editor: supersimple.editor.html }, html.evalCons));
