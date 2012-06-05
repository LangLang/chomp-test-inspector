var Editor = Editor || {};
(function(html){
  var processElement = adt({
    _: function(attributes) { console.log("Unknown DOM element ", this._tag, this._datatype, attributes); }
  });

  Editor.highlight = function(domElement) {
    var child = domElement.firstChild;
    while (child) {
      processElement(child);
      child = child.nextSibling;
    }
  };
})(html.evalCons);