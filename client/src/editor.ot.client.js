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
    OTClient.prototype.sendOperation = function (operation) {
      console.log("SEND OPERATION", operation);
      //this.channel.write(operation);
    };

    OTClient.prototype.applyOperation = function (operation) {
      console.log("APPLY OPERATION", this, operation);
      //this.str = operation.apply(this.str);
    };
    return OTClient;
  })(ot.Client);
