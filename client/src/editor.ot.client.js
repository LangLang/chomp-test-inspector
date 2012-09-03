  var Editor = Editor || {};
  Editor.OTClient = (function(_super) {
    // Generic (CoffeeScript style) inherritance routines
    var 
      __hasProp = {}.hasOwnProperty,
      __extends = function(child, parent) {
        for (var key in parent)
          if (__hasProp.call(parent, key))
            child[key] = parent[key];
        function ctor() { this.constructor = child; }
        ctor.prototype = parent.prototype;
        child.prototype = new ctor();
        child.__super__ = parent.prototype;
        return child;
      };

    // Inherrit editor client from ot.Client
    __extends(OTClient, _super);
    function OTClient(file) {
      this.file = file;
      return OTClient.__super__.constructor.apply(this, Array.prototype.slice.call(arguments, 1));
    }

    // Override methods
    OTClient.prototype.constructor = OTClient;

    OTClient.prototype.createOperation = function () {
      return new Editor.OTOperation(this.callMethodForState('newRevision'));
    };

    OTClient.prototype.sendOperation = function (operation) {
      var 
        i,
        op,
        actions = [],
        message;
      
      // Serialize message
      for (i = 0; i < operation.ops; ++i) {
        op = operation.ops[i];
        if (op.retain)
          actions.push(OTAction.Retain(op.retain));
        else if (op.delete)
          actions.push(OTAction.Delete(op.delete));
        else
          actions.push(OTAction.Insert(op.insert));
      }
      message = Message.OperationalTransform(this.file, operation.revision, actions);

      // Send the serialized message to the server
      MessageService.send(state.ws, message);
    };

    OTClient.prototype.applyOperation = function (operation) {
      var
        text = Editor.read(this.file),
        transformedText = operation.apply(text);
      Editor.update(this.file, transformedText);
    };

    return OTClient;
  })(ot.Client);
