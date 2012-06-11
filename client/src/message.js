  var 
    Message = adt('Acknowledge','Notify','ReloadFiles','LoadFile','UnloadFile','PatchFile','ParseError'),
    // Not used: ReloadWatchPath
    StorageEvent = adt(
      'Connected',
      'ModifiedFile',
      'ModifiedDirectory',
      'MovedOutFile',
      'MovedInFile',
      'RenamedFile',
      'MovedOutDirectory',
      'MovedInDirectory',
      'RenamedDirectory',
      'MovedOutRootDirectory',
      'RestoredRootDirectory',
      'CreatedFile',
      'CreatedDirectory',
      'DeletedFile',
      'DeletedDirectory',
      'DeletedRootDirectory',
      'UnmountedRootDirectory',
      'Error'
    );