  var LangLang = LangLang || {};
  (function(html){
    /* TODO: move partial to adt.partial
    var partial = function(data) { return function() { 
      dataNew = data.concat(arguments);
      dataNew._tag = data._tag;
      return dataNew;
    }; };//*/
    var highlighter = adt.recursive(adt({
      String: html.span,
      _: function() { 
        var args = Array.prototype.slice.call(arguments, 0);
        return html.span.apply(null, [{ class: 'syntax-' + this._tag }].concat(args));
      }
    }));
    LangLang.highlight = function(str, caretPos) {
      var 
        parseResult = LangLang.parse(str, caretPos),
        highlightResult = { html: [], caretPos: parseResult.caretPos },
        i;
      for (i = 0; i < parseResult.ast.length; ++i)
        highlightResult.html.push(highlighter(parseResult.ast[i]));
      return highlightResult;
    };
  })(html.evalCons);