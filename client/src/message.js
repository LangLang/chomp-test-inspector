  var 
    Message = adt('Acknowledge','Notify','ReloadFiles','LoadFile','PatchFile','ParseError'),
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
      'MovedRootDirectory',
      'CreatedFile',
      'CreatedDirectory',
      'DeletedFile',
      'DeletedDirectory',
      'DeletedRootDirectory',
      'UnmountedRootDirectory',
      'Error'
    );