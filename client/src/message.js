  var 
    Message = adt('Acknowledge','Notify','ReloadFiles','LoadFile','UnloadFile','PatchFile','ParseError'),
    StorageEvent = adt(
      'WatchInstalled',
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
    ),
    ProcessLog = adt('LogStart', 'LogInfo', 'LogError', 'LogEnd');
