  var StatusBar = StatusBar || {};
  
  (function(){
  
    var storageEventHandler = adt({
      MovedRootDirectory: function() {
        $('#page-statusbar')
        .text("The root directory has been moved")
        .removeClass('flash-info')
        .addClass('flash-info')
      },
      DeletedRootDirectory: function() {
        $('#page-statusbar')
        .removeClass('flash-info')
        .addClass('flash-info')
      }
    });

    StatusBar.handler = adt({
      ReloadFiles: function(storageEvent, files) { storageEventHandler(storageEvent); }
    });
  })();