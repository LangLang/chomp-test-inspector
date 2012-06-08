  var LangLang = LangLang || {};
  (function(){
    var 
      getTag = adt({_: function(){ return this._tag; } }),
      lexCons = adt(
        '#',
        ':', '⋅', '\\', '\\\\',
        '->', '→', '→.', '→:', '→\\', '→\\\\',
        '(', ')',
        '()', '_'
      ),
      lexReplace = function(lexeme) {
        switch (lexeme) {
          case '\r': return '';
          case '\t': return '  ';
        }
        return lexeme;
      },
      lexReduce = function(left, right) {
        if (left == null)
          return [right];
        var
          lexReduceHash = {
            '->': '→',
            '\\\\': '\\\\',
            '→.': '→.',
            '→:': '→:',
            '→\\': '→\\',
            '→\\\\': '→\\\\',
            '()': '()'
          },
          result = lexReduceHash[left + right];
        if (result)
          return [result];
        if (left in lexCons || right in lexCons)
          return [left, right];
        switch(left) {
          case ' ': case '\n': return [left, right];
        }
        switch(right) {
          case ' ': case '\n': return [left, right];
        }
        return [left + right];
      },
      // TODO: pragma
      astCons = adt('space', 'eol', 'error', 'lbracket', 'rbracket', 'comment', 'arrow', 'operator', 'def', 'expr', 'identifier', 'top', 'bottom'),
      astReplace = function(lexeme) {
        var 
          astReplaceHash = {
            ' ': astCons.space(' '),
            '\n': astCons.eol('\n'),
            '#': astCons.comment('#'),
            '.': astCons.operator('.'),
            ':': astCons.operator(':'),
            '\\': astCons.operator('\\'),
            '\\\\': astCons.operator('\\\\'),
            '→.': astCons.arrow('→.'),
            '→:': astCons.arrow('→:'),
            '→\\': astCons.arrow('→\\'),
            '→\\\\': astCons.arrow('→\\\\'),
            '(': astCons.lbracket('('),
            ')': astCons.rbracket(')'),
            '()':  astCons.bottom('()'),
            '_':  astCons.top('_'),
            'error':  astCons.error('error')
          },
          result = astReplaceHash[lexeme];
        if (result)
          return [result];
        return astCons.identifier(lexeme);
      },
      astReduceEval = adt({
        'whitespace whitespace': function(l,r) {
          return [astCons.whitespace(l[0]+r[0])]; 
        },
        'comment identifier': function(comment, identifier) { 
          if (comment[comment.length-1] == '\n')
            return [astCons.comment(comment), astCons.token(token)];
          return astCons.comment(comment + token);
        },
        /*'identifier identifier': function() {

        },
        'token whitespace': function() {
          
        },
        'token arrow': function() {
          //TODO: [token whitespace arrow]
        },
        'token whitespace arrow': function() {
          
        },
        '→.': '→.',
        '→:': '→:',
        '→\\': '→\\',
        '→\\\\': '→\\\\',
        '_': '_'*/
      }),
      astReduce = function(left, right) {
        if (left == null)
          return [right];
        var
          generateTag = adt({_: function(){ return this._tag; }}),
          leftTag = generateTag(left),
          generateTag = generateTag(right),
          result = astReduceEval[leftTag + ' ' + rightTag];
        return result? [result(left, right)] : [left, right];
        //'token→': '→',
        //'function→': '→',

      };
      
    LangLang.parse = function(str) {
      var 
        lexResult = [""], 
        lexHead,
        lexeme,
        //astStack = [],
        astResult = [], 
        astHead,
        i, j,
        left, right;

      var parse = function(lexeme) {
        var 
          left = astResult.length > 1? astResult.pop() : (void 0),
          right = astReplace(lexeme);
        // TODO: run astReduce on multiple arguments
        astResult = astReduce(left, right);
      };

      for (i = 0; i < str.length; ++i) {
        lexeme = lexReplace(str[i]);
        for (j = 0; j < lexeme.length; ++j) {
          lexHead = lexResult.pop();
          lexResult = lexResult.concat(lexReduce(lexHead, lexeme[j]));
        }
        while (lexResult.length > 2)
          parse(lexResult.pop());
      }
      if (lexResult.length == 1)
        parse(lexResult.pop());
      return [astResult];
    };

  })();