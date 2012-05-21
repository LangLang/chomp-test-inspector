/*
 * supersimple-editor.js - A super... simple... editor (in js)
 * supersimple-editor.js is free, public domain software (http://creativecommons.org/publicdomain/zero/1.0/)
 * Originally created by Rehno Lindeque of http://www.mischievousmeerkat.com
 */
var supersimple = supersimple || {};
(function() {
"use strict";
  // Core goes here
  var
    Editor = function(textContents) {
      this.el = html.evalCons.pre(
        {class: "supersimple-editor-input", contenteditable: "true", style: "padding: 20px;background: #ddd; outline: none;"}, 
        typeof textContents === 'string'? textContents : "");
      // Prevent formatting keys:
      this.keys = { ctrl: false };
      (function(self){
        self.el.onkeydown = function(e) {
          var 
            code = e.charCode? e.charCode : e.keyCode,
            c = String.fromCharCode(code);
          if (e.ctrlKey)
            self.keys.ctrl = true;
          if (self.keys.ctrl && (c === 'B' || c === 'I'))
            return false;
          return true;
        };
        self.el.onkeyup = function(e) {
          if (e.ctrlKey)
            self.keys.ctrl = false;
          return true;
        };
        self.el.onblur = self.el.onfocus = function(e) {
          self.keys.ctrl = false;
          return true;
        };
      })(this);
    };

  supersimple.editor = function() {
    return new Editor();
  };
  // Export supersimple to a CommonJS module if exports is available
  if (typeof(exports) !== "undefined" && exports !== null)
    exports.module = supersimple.editor;
  return;
})();
