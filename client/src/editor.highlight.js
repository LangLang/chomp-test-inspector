  var Editor = Editor || {};
  (function(html){
    /*var processElement = adt({
      Text: function(el) {
        var ast = LangLang.highlight(el.data);
        console.log("LangLang AST: ", ast);
      },
      _: function(attributes) { console.log("Unknown DOM element ", this._tag, this._datatype, attributes); }
    });*/

    var 
      getNodeOffset = function(childNode) {
        var prevNode = childNode.previousSibling;
        if (!prevNode)
          return 0;
        return prevNode.textContent.length + getNodeOffset(prevNode);
      },
      getParentNodeOffset = function(parentNode, childNode) {
        var offset = getNodeOffset(childNode);
        if (childNode.parentNode === parentNode)
          return offset;
        return offset + getParentNodeOffset(parentNode, childNode.parentNode);
      },
      getCaretOffset = function(domElement) {
        if (!window.getSelection)
          return 0;
        var selection = window.getSelection();
        if (selection.focusNode === domElement)
          return selection.focusOffset;
        return getParentNodeOffset(domElement, selection.focusNode) + selection.focusOffset;
      },
      setCaretOffset = function() {
        if (window.getSelection) {
          var s = window.getSelection();
          if (s.rangeCount > 0) 
            s.removeAllRanges();
          s.addRange(savedRange);
        }
        else 
          if (document.createRange)
            window.getSelection().addRange(savedRange);
      };


    Editor.highlight = function(domElement) {
      var 
        result = LangLang.highlight(domElement.textContent),
        i;
      console.log(result);
      console.log("Caret offset: ", getCaretOffset(domElement));
      domElement.innerHTML = "";
      for (var i = 0; i < result.length; ++i)
        domElement.appendChild(result[i]);
      /*
      var child = domElement.firstChild;
      while (child) {
        processElement(child);
        child = child.nextSibling;
      }*/
    };
  })(html.evalCons);