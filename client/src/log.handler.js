  var Log = Log || {};
  
  (function(){
  
    var show = adt({_: function(){ return this._tag; } });

    Log.handler = adt({
      Acknowledge: function() { console.log("\t...previous message acknowledged"); },
      Notify: function(notification) { console.log("\t...notification (" + show(notification) + "): ", notification); },
      ReloadFiles: function(storageEvent, files) {
        console.log("\t...reload files (" + show(storageEvent) + "): ", files);
      },
      LoadFile: function(storageEvent, file) { 
        console.log("\t...load file (" + show(storageEvent) + "): ", file);
      },
      UnloadFile: function(storageEvent, file) {
        console.log("\t...unload file (" + show(storageEvent) + "): ", file);
      },
      LoadFileContents: function(file, revision, contents) { 
        console.log("\t...load file contents: ", file, " (revision " + String(revision) + ")");
      },
      UnloadFileContents: function(file) { 
        console.log("\t...unload file contents: ", file);
      },
      OperationalTransform: function(file, revision, actions) {},
      ParseError: function(message) {
        console.error("\t...previous message this client sent could not be parsed: \n", message); 
      },
      ConnectionClosed: function() {},
      _: function() { console.error("\t...(error) unknown message type `" + this._tag + "`" ); }
    });
  })();