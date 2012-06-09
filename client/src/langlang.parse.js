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
        // Pre-condition: lexeme.length > 0
        var 
          astReplaceHash = {
            ' ': astCons.space(' '),
            '\n': astCons.eol('\n'),
            '#': astCons.comment('#'),
            '.': astCons.operator('.'),
            ':': astCons.operator(':'),
            '\\': astCons.operator('\\'),
            '\\\\': astCons.operator('\\\\'),
            '→': astCons.arrow('→'),
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
          return result;
        return astCons.identifier(lexeme);
      },
      astReduceEval = adt({
        'identifier space arrow': function(a,b,c) {
          return astCons.def(a,b,c);
        },
        'def space identifier': function(a,b,c) {
          return astCons.def.apply(null, a.concat([b,c])); 
        },
        'def space top': function(fl,l,r) {
          return astCons.def.apply(null, a.concat([b,c])); 
        },
        'def space bottom': function(fl,l,r) {
          return astCons.def.apply(null, a.concat([b,c])); 
        },
        'def space error': function(fl,l,r) {
          return astCons.def.apply(null, a.concat([b,c])); 
        }
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
      astReduce = function(farLeft, left, right) {
        // TODO: refactor (use variable number of arguments)
        if (left == null)
          return [right];
        var
          generateTag = adt({_: function(){ return this._tag; }}),
          leftTag = generateTag(left),
          farLeftTag = farLeft? generateTag(farLeft) : "",
          rightTag = generateTag(right),
          result = astReduceEval[farLeftTag + ' ' + leftTag + ' ' + rightTag],
          resultArgs = farLeft? [farLeft, left, right] : [left, right];
        return result? [result.apply(null, resultArgs)] : resultArgs;
        //'token→': '→',
        //'function→': '→',s
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
        if (lexeme == null || lexeme.length == 0)
          return;
        var 
          left = astResult.length > 0? astResult.pop() : (void 0),
          right = astReplace(lexeme),
          farLeft,
          r;
        // Run astReduce on up to three arguments (i.e. maximum lookahead of 2)
        // Try to reduce 2
        // TODO: refactor (undefined is unnecessary)
        r = astReduce((void 0), left, right);
        // Try to reduce 3
        if (r.length > 1) {
          farLeft = astResult.length > 0? astResult.pop() : (void 0);
          r = astReduce(farLeft, left, right);
        }
        astResult = astResult.concat(r);
      };

      for (i = 0; i < str.length; ++i) {
        lexeme = lexReplace(str[i]);
        for (j = 0; j < lexeme.length; ++j) {
          lexHead = lexResult.pop();
          lexResult = lexResult.concat(lexReduce(lexHead, lexeme[j]));
        }
        while (lexResult.length > 1)
          parse(lexResult.shift());
      }
      if (lexResult.length == 1)
        parse(lexResult.shift());
      return astResult;
    };

  })();