  var StatusBar = StatusBar || {};
  
  (function(){
  
    var storageEventHandler = adt({
      MovedRootDirectory: function() {
        $('#page-statusbar')
        .text("The root directory has been moved")
        .addClass('show-statusbar');
      },
      DeletedRootDirectory: function() {
        $('#page-statusbar')
        .text("The root directory has been deleted")
        .addClass('show-statusbar');
      }
    });

    StatusBar.handler = adt({
      ReloadFiles: function(storageEvent, files) { storageEventHandler(storageEvent); }
    });
  })();