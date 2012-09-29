  var StatusBar = StatusBar || {};  
  (function(html){
    StatusBar.handler = adt({
      ReloadFiles: function(storageEvent, files) { 
        adt({
          MovedOutRootDirectory: function() { StatusBar.important("The root directory was moved"); },
          RestoredRootDirectory: function() { StatusBar.info("The root directory has been restored"); },
          DeletedRootDirectory:  function() { StatusBar.important("The root directory was deleted"); }
        })(storageEvent);
      },
      LoadFile: function(storageEvent, file) { 
        adt({
          CreatedFile: function() { StatusBar.info("The file ", html.code(file), " was created"); },
          MovedInFile: function() { StatusBar.info("The file ", html.code(file), " has been moved into the project"); }
        })(storageEvent);
      },
      UnloadFile: function(storageEvent, file) { 
        adt({
          DeletedFile: function() { StatusBar.info("The file ", html.code(file), " has been deleted"); },
          MovedOutFile: function() { StatusBar.info("The file ", html.code(file), " was moved out of the project"); }
        })(storageEvent);
      },
      LoadFileContents: function(file, revision, contents) {},
      UnloadFileContents: function(file) {},
      /*
      RenamedFile: function(sourceFile, targetFile) {
        StatusBar.info("The file ", html.code(sourceFile), " has been renamed to ", html.code(targetFile));
      },//*/
      OperationalTransform: function(file, revision, actions) {},
      ParseError: function(message) {},
      ConnectionClosed: function() { StatusBar.important("The connection to the server was closed."); }
    });
  })(html);