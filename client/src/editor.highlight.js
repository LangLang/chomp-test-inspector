  var Editor = Editor || {};
  (function(html){
    Editor.highlight = function(file) {
      var 
        domElement = Editor.DOM.getInput(file),
        originalCaretPos = Editor.getCaretOffset(domElement),
        caret = (function(caretPos){
          var 
            caretP = caretPos, 
            streamP = 0;
          return {
            retain: function(n) { streamP += n; },
            insert: function(str) { 
              if (streamP <= caretP)
                caretP += str.length;
              streamP += str.length;
            },
            backspace: function(str) {
              var 
                streamAhead = Math.max(streamP - caretP, 0),
                d = Math.max(str.length - streamAhead, 0);
              streamP -= str.length;
              caretP -= d;
            },
            'delete': function(str) {
              var
                caretAhead = Math.max(caretP - streamP, 0),
                d = Math.min(str.length, caretAhead);
              caretP -= d;
            },
            getPosition: function() { return caretP; }
          };
        })(originalCaretPos),
        otClient = Editor.getOTClient(file),
        otOperations = otClient.createOperation(),
        opHandler = {
          retain: function(n)     { 
            caret.retain(n);
            otOperations.retain(n); },
          insert: function(str)   {
            caret.insert(str); 
            otOperations.insert(str); },
          backspace: function(str){ caret.backspace(str); otOperations.backspace(str); },
          delete: function(str)   { caret.delete(str); otOperations.delete(str); }
        },
        result = LangLang.highlight((domElement.innerText? domElement.innerText : domElement.textContent), opHandler),
        i;
      domElement.innerHTML = "";
      for (var i = 0; i < result.html.length; ++i)
        domElement.appendChild(result.html[i]);
      if (originalCaretPos != null)
        Editor.setCaretOffset(domElement, caret.getPosition());
      otClient.applyClient(otOperations);
      return result;
    };
  })(html.evalCons);