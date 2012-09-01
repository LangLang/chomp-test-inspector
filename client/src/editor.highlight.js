  var Editor = Editor || {};
  (function(html){
    var
      getTextContent = function(domElement) {
        return domElement.innerText? domElement.innerText : domElement.textContent;
      },
      getNodeOffset = function(childNode) {
        var 
          prevNode = childNode.previousSibling,
          offset = 0;
        // If this is a div then it moves children and next siblings to the next line
        // (Chromium inserts divs for newlines with contenteditable elements)
        if (childNode["tagName"] && childNode["tagName"] === 'DIV')
          ++offset;
        if (!prevNode)
          return offset;
        // The previous node may contain a <br> tag if it is completely empty...
        // Therefore, it is not possible to use getTextContent here because it
        // will add a second newline to the counter in this case.
        // TODO: This may still be broken when pasting text into the editor...
        //       because it assumes that the <br> element is followed by a <div>
        //       element.
        //return offset + getTextContent(prevNode).length + getNodeOffset(prevNode);        
        return offset + prevNode.textContent.length + getNodeOffset(prevNode);
      },
      getParentNodeOffset = function(parentNode, childNode) {
        var offset = getNodeOffset(childNode);
        if (childNode.parentNode === parentNode)
          return offset;
        // If this is a div then it moves children and next siblings to the next line
        // (Chromium inserts divs for newlines with contenteditable elements)
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
          return [rootNode, Math.min(offset, getTextContent(rootNode).length)];
        while (childNode) {
          // Skip nodes that fall behind the offset (calculate offset to parent node)
          textLength = getTextContent(childNode).length;
          if (parentOffset + textLength <= offset) {
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
      getFocusInSelection = function(selection, node) {
          var 
            text = getTextContent(selection.focusNode),
            focusOffset = selection.focusOffset,
            i;
          /* Count the number of newlines before the caret offset inside this node
          console.log("FOCUS NODE", selection.focusNode, selection.focusOffset);
          for (i = 0; i < focusOffset; ++i) {
            if (text[i] === '\n') {
              console.log("NEWLINE");
              ++focusOffset;
            }
          }
          if (focusOffset == 0) {
            // Google-chrome inserts divs in order to create new-lines... so in
            // the case that we're inside a div, add one to the focus offset
            console.log(selection.focusNode.parentNode.parentNode.parentNode);
            //for (var j = 0; j < selection.focusNode.parentNode.parentNode.parentNode.parentNode.children.length; ++j)
            //  console.log(selection.focusNode.parentNode.parentNode.parentNode.parentNode.children[j]);
          }
          console.log("        --offset: ", focusOffset);*/
          return focusOffset;
      },
      getCaretOffset = function(domElement) {
        if (!window.getSelection)
          return null;
        var selection = window.getSelection();
        if (!selection.containsNode(domElement, true)) 
          return null;
        if (selection.focusNode === domElement)
          return getFocusInSelection(selection);
        if (selection.focusNode == null)
          return null;
        return getParentNodeOffset(domElement, selection.focusNode) + getFocusInSelection(selection);
      },
      setCaretOffset = function(domElement, offset) {
        if (!window.getSelection)
          return;
        var
          selection = window.getSelection(),
          focusPoint = getFocusPointAtOffset(domElement, offset),
          range = document.createRange();
        // Remove existing selections
        if (selection.rangeCount > 0)
          selection.removeAllRanges();
        // Set the new selection to the focus point
        range.setStart(focusPoint[0], focusPoint[1]);
        range.setEnd(focusPoint[0], focusPoint[1]);
        selection.addRange(range);
      };

    Editor.highlight = function(file) {
      var 
        domElement = Editor.DOM.getInput(file),
        originalCaretPos = getCaretOffset(domElement),
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
        result = LangLang.highlight(getTextContent(domElement), opHandler),
        i;
      domElement.innerHTML = "";
      for (var i = 0; i < result.html.length; ++i)
        domElement.appendChild(result.html[i]);
      if (originalCaretPos != null)
        setCaretOffset(domElement, caret.getPosition());
      console.log("OT OPERATIONS:",otOperations);
      return result;
    };
  })(html.evalCons);