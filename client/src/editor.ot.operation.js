  var Editor = Editor || {};
  Editor.OTOperation = (function(_super) {
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

    // Inherrit editor client from ot.Operation
    __extends(OTOperation, _super);
    function OTOperation(revision, id, meta) {
      return OTOperation.__super__.constructor.apply(this, Array.prototype.slice.call(arguments, 0));
    }

    // Override methods
    OTOperation.prototype.constructor = OTOperation;
    OTOperation.prototype.backspace = function (str) {
      // This operation could be provided by simply merging two sets of operations, however this method is commonly used
      // in the editor and should be quite a bit faster
      assert(typeof str === 'string');
      if (str.length === 0) { return this; }
      assert(this.targetLength >= str.length);
      this.targetLength -= str.length;
      var
        remaining = str.length,
        lastOp = this.ops[this.ops.length-1],
        followOps = [],
        d = 0,
        dd,
        dStr = [];
      while (lastOp && remaining > 0) {
        if (lastOp.retain) {
          if (lastOp.retain <= remaining) {
            dd = lastOp.retain;
            remaining -= lastOp.retain;
            this.ops.pop();
          }
          else {
            lastOp.retain -= remaining;
            dd = remaining;
            remaining = 0;
          }
          d += dd;
          dStr.push(str.slice(remaining, remaining + dd));
        }
        else if (lastOp.insert) {
          if (lastOp.insert <= remaining) {
            dd = lastOp.insert;
            remaining -= lastOp.insert;
            this.ops.pop();
          }
          else {
            lastOp.insert -= remaining;
            dd = remaining;
            remaining = 0;
          }
          d += dd;
        }
        else if (lastOp.delete) {
          dd = lastOp.delete.length;
          dStr.push(lastOp.delete);
          this.ops.pop();
          d += dd;
        }
        lastOp = this.ops[this.ops.length-1];
      }

      // Create a new delete op
      if (d > 0)
        this.ops.push({ 'delete': dStr });
      
      return this;
    };

    return OTOperation;
  })(ot.Operation);
