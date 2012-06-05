  var StatusBar = StatusBar || {};  
  (function(){  
    var storageEventHandler = adt({
      MovedOutRootDirectory: function() { StatusBar.important("The root directory was moved"); },
      RestoredRootDirectory: function() { StatusBar.info("The root directory has been restored"); },
      DeletedRootDirectory:  function() { StatusBar.important("The root directory was deleted"); }
    });

    StatusBar.handler = adt({
      ReloadFiles: function(storageEvent, files) { storageEventHandler(storageEvent); }
    });
  })();