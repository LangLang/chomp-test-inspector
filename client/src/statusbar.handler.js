  var StatusBar = StatusBar || {};  
  (function(){  
    var storageEventHandler = adt({
      MovedRootDirectory: function() { StatusBar.info("The root directory was moved"); },
      DeletedRootDirectory:  function() { StatusBar.info("The root directory was deleted"); }
    });

    StatusBar.handler = adt({
      ReloadFiles: function(storageEvent, files) { storageEventHandler(storageEvent); }
    });
  })();