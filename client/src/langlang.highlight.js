  var LangLang = LangLang || {};
  (function(html){
    var highlighter = adt.recursive(adt({
      identifier: html.strong,
      _: html.span
    }));
    LangLang.highlight = function(str) {
      var 
        ast = LangLang.parse(str),
        i, 
        result = [];
      for (i = 0; i < ast.length; ++i)
        result.push(highlighter(ast[i]));
      return result;
    };
  })(html.evalCons);