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
            backspace: function(n) {
              var 
                streamAhead = Math.max(streamP - caretP, 0),
                d = Math.max(n - streamAhead, 0);
              streamP -= n;
              caretP -= d;
            },
            'delete': function(n) {
              var
                caretAhead = Math.max(caretP - streamP, 0),
                d = Math.min(n, caretAhead);
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
          backspace: function(n){ caret.backspace(n); otOperations.backspace(n); },
          delete: function(n)   { caret.delete(n); otOperations.delete(n); }
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