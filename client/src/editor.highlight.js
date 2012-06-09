  var Editor = Editor || {};
  (function(html){
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
      getFocusPointAtOffset = function(rootNode, offset) {
        var
          childNode = rootNode.firstChild,
          parentOffset = 0,
          lastChildNode,
          textLength,
          descendant;
        if (offset == 0)
          return [rootNode, 0];
        if (childNode === null)
          return [rootNode, Math.min(offset, rootNode.textContent.length)];
        while (childNode) {
          // Skip nodes that fall behind the offset (calculate offset to parent node)
          textLength = childNode.textContent.length;
          if (parentOffset + textLength < offset) {
            parentOffset += textLength;
            childNode = childNode.nextSibling;
            continue;
          }
          // The node containing the offset is some descendant of this childNode
          descendant = getFocusPointAtOffset(childNode, offset - parentOffset);
          return descendant;
        }
        // Offset is past the end of the text content
        return getFocusPointAtOffset(rootNode.lastChild, offset);
      },
      getCaretOffset = function(domElement) {
        if (!window.getSelection)
          return 0;
        var selection = window.getSelection();
        if (selection.focusNode === domElement)
          return selection.focusOffset;
        return getParentNodeOffset(domElement, selection.focusNode) + selection.focusOffset;
      },
      setCaretOffset = function(domElement, offset) {
        var
          selection = window.getSelection(),
          focusPoint = getFocusPointAtOffset(domElement, offset),
          range = document.createRange();
        // Remove existing selections
        if (window.getSelection)
          if (selection.rangeCount > 0) 
            selection.removeAllRanges();
        range.setStart(focusPoint[0], focusPoint[1]);
        range.setEnd(focusPoint[0], focusPoint[1]);
        selection.addRange(range);
      };

    Editor.highlight = function(domElement) {
      var 
        result = LangLang.highlight(domElement.textContent),
        i,
        caretOffset = getCaretOffset(domElement);
      domElement.innerHTML = "";
      for (var i = 0; i < result.length; ++i)
        domElement.appendChild(result[i]);
      setCaretOffset(domElement, caretOffset);
    };
  })(html.evalCons);