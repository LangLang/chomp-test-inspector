  var Log = Log || {};
  Log.handler = adt({
    Acknowledge: function() { console.log("...previous message acknowledged"); },
    Notify: function(notification) { console.log("...notification:", notification); },
    ReloadFiles: function(files) { 
      console.log("...reload files: ", files);
    },
    PatchFile: function(filePath, patch) {
      console.log("...apply patch to file: ", file);
      console.log("...patch: ", patch);
    },
    ParseError: function(message) { console.error("...previous message this client sent could not be parsed: \n", message); },
    _: function() { console.error("...(error) unknown message type `" + this._tag + "`" ); }
  });
