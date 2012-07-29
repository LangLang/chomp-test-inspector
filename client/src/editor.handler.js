  var Editor = Editor || {};
  (function(html){
    var 
      // Operational transform clients 
      // Keeps track of the synchronization state (synchronized/awaitingConfirm/awaitingWithBuffer) and revision number of a file
      otClients = {},
      createDropDownLog = function() {
        return html.div({class: "editor-log editor-log-empty"}, 
          html.div({class: "editor-log-dropdown"}),
          html.div({class: "editor-log-lines"})
        );
      },
      createEditor = function(fileName) {
        return html.div({class: "editor", 'data-filename': fileName},
          html.div({class: "editor-source editor-unloaded"},
            html.input({class: "editor-source-filename", value: fileName}),
            html.editor()
          ),
          html.div({class: "editor-result editor-unloaded"},
            html.input({class: "editor-result-filename", readonly: "readonly", value: fileName + ".output"}),
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
      }),
      isResultFile = function(file) { return /\.output$/.test(file); },
      loadFile = function(file) {
        var
          isResult = isResultFile(file),
          baseFilename = isResult? file.slice(0, file.length - ".output".length) : file,
          $editors = $("#editors"),
          $editor = $editors.find(".editor[data-filename='" + baseFilename + "']"),
          selectorPrefix = isResult? ".editor-result" : ".editor-source",
          $editorFilename;
        if ($editor.length == 0)
          $editor = $(createEditor(baseFilename)).appendTo($editors);
        $editorFilename = $editor.find(selectorPrefix + "-filename"),
        $editor
          .find(selectorPrefix)
          .removeClass("editor-unloaded")
          .find(selectorPrefix + "-input")
          .empty()
      };

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
        var i, f;
        $("#editors").html("");
        otClients = {};
        for (i = 0; i < files.length; ++i)
          loadFile(files[i]);
      },
      LoadFile: function(storageEvent, file) {
        delete otClients[file];
        loadFile(file);
      },
      LoadFileContents: function(file, maybeContents) {
        var
          isResult = isResultFile(file),
          baseFilename = isResult? file.slice(0, file.length - ".output".length) : file,
          $editors = $("#editors"),
          $editor = $editors.find(".editor[data-filename='" + baseFilename + "']"),
          selector = isResult? ".editor-result" : ".editor-source",
          $editorInput = $editor.find(selector).find(".supersimple-editor-input");

        if ($editor.length !== 1 || $editorInput.length !== 1)
          return;
        adt.recursive(adt({
          FileContents: function(contents, revision) { 
            otClients[file] = new Editor.OTClient(file, revision);
            $editorInput
              .text(contents)
              .attr('contenteditable', !isResult);
            Editor.highlight($editorInput.get(0));
          },
          Nothing: function() {
            delete otClients[file];
            $editorInput
              .text("")
              .attr('contenteditable', false);
          }
        }))(maybeContents);
      },
      UnloadFile: function(storageEvent, file) { 
        var
          isResult = isResultFile(file),
          baseFilename = isResult? file.slice(0, file.length - ".output".length) : file,
          $editors = $("#editors"),
          $editor = $editors.find(".editor[data-filename='" + baseFilename + "']"),
          thisPrefix = isResult? ".editor-result" : ".editor-source",
          otherPrefix = isResult? ".editor-source" : ".editor-result";
        delete otClients[file];
        if ($editor.length == 0)
          return;
        if ($editor.find(otherPrefix).hasClass('editor-unloaded'))
          $editor.remove();
        else
          $editor
            .find(thisPrefix).addClass('editor-unloaded')
            .find(".supersimple-editor-input")
              .text("")
              .attr('contenteditable', false);
      },
      OperationalTransform: function(file, actions) {
        // TODO: Apply operational transform
      },
      ConnectionClosed: function() {
        otClients = {};
        $("#editors").html("");
        disableEditor();
      }
    }));
  })(adt({ editor: supersimple.editor.html }, html.evalCons));
