  var Log = Log || {};
  
  (function(){
  
    var show = adt({_: function(){ return this._tag; } });

    Log.handler = adt({
      Acknowledge: function() { console.log("...previous message acknowledged"); },
      Notify: function(notification) { console.log("...notification (" + show(notification) + "): ", notification); },
      _: function() { console.error("...(error) unknown message type `" + this._tag + "`" ); }
      ReloadFiles: function(storageEvent, files) {
        console.log("...reload files (" + show(storageEvent) + "): ", files);
      },
      LoadFile: function(storageEvent, file) { 
        console.log("...load file (" + show(storageEvent) + "): ", file);
      },
      LoadFileContents: function(file, contents) { 
        console.log("...load file contents: ", file);
      },
      UnloadFile: function(storageEvent, file) {
        console.log("...unload file (" + show(storageEvent) + "): ", file);
      },
      OperationalTransform: function() {},
      ParseError: function(message) {
        console.error("...previous message this client sent could not be parsed: \n", message); 
      },
      ConnectionClosed: function() {},
      _: function() { console.error("...(error) unknown message type `" + this._tag + "`" ); }
    });
  })();