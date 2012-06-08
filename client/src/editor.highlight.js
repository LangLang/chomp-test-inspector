  var Editor = Editor || {};
  (function(html){
    var processElement = adt({
      Text: function(el) {
        var ast = LangLang.parse(el.data);
        console.log("LangLang AST: ", ast);
      },
      _: function(attributes) { console.log("Unknown DOM element ", this._tag, this._datatype, attributes); }
    });

    Editor.highlight = function(domElement) {
      var child = domElement.firstChild;1
      while (child) {
        processElement(child);
        child = child.nextSibling;
      }
    };
  })(html.evalCons);