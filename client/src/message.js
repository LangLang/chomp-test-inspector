  var
    Message = adt(
      // Client-server messages
      'Acknowledge','Notify','ReloadFiles','LoadFile','UnloadFile','OperationalTransform','ParseError', 
      // Other messages (not used on server)
      'ConnectionClosed'),
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
    ProcessLog = adt('LogStart', 'LogInfo', 'LogError', 'LogEnd'),
    OTAction = adt(
      'Retain', // Int
      'Insert', // Text
      'Delete'  // Int
    );
