  var LangLang = LangLang || {};
  (function(html){
    var highlighter = adt.recursive(adt({
      identifier: html.strong,
      _: html.span
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