  var LangLang = LangLang || {};
  (function(){
    var
      getTag = adt({_: function(){ return this._tag; } }),
      lexCons = adt(
        '#',
        ':', '.', '\\', '\\\\',
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
        if (left == null || left.length == 0)
          return [right];
        if (right.length == 0)
          return [left];
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
        'def space top': function(a,b,c) {
          return astCons.def.apply(null, a.concat([b,c])); 
        },
        'def space bottom': function(a,b,c) {
          return astCons.def.apply(null, a.concat([b,c])); 
        },
        'def space error': function(a,b,c) {
          return astCons.def.apply(null, a.concat([b,c])); 
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
      astReduce = function(farLeft, left, right) {
        // TODO: refactor (use variable number of arguments)
        if (left == null)
          return [right];

        // Reduce comments first
        // TODO: This should be done by pattern matching instead in the future...
        if (left._tag === 'comment' && right._tag !== 'eol')
          return [astCons.comment(left.concat(right).join(''))];

        // Reduce all other ast combinations
        var
          generateTag = adt({_: function(){ return this._tag; }}),
          leftTag = generateTag(left),
          farLeftTag = farLeft? generateTag(farLeft) : "",
          rightTag = generateTag(right),
          result = astReduceEval[farLeftTag + ' ' + leftTag + ' ' + rightTag],
          resultArgs = farLeft? [farLeft, left, right] : [left, right];
        return result? [result.apply(null, resultArgs)] : resultArgs;
      };
    
    LangLang.parse = function(input, opHandler) {
      // Extra operations to perform during lexical analysis (e.g. maintain caret position and generate transform operations)
      var
        replace = function(lexeme) {
          var result = lexReplace(lexeme);
          if (result == lexeme) {
            opHandler.retain(result.length);
          }
          else {
            opHandler.delete(lexeme);
            opHandler.insert(result);
          }
          return result;
        },
        reduce = function(left, right) {
          var 
            i,
            sourceStr = left + right,
            result = lexReduce(left, right),
            resultStr = result.join(''),
            insertChars;
          
          if (resultStr == sourceStr)
            return result;
          // Get the longest common subsequence on the left of the result
          for (i = 0; i < resultStr.length; ++i)
            if (i == sourceStr.length || resultStr[i] != sourceStr[i])
              break;
          opHandler.backspace(sourceStr.slice(i));
          opHandler.insert(resultStr.slice(i));
          return result;
        };

      // Main lex analysis loop
      var
        lexResult = [""], 
        lexHead,
        lexeme,
        srcHead,
        src,
        lexReduction,
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

      for (i = 0; i < input.length; ++i) {
        // Replace simple characters in the input stream (producing tiny "lexemes" - mostly characters)
        lexeme = replace(input[i]);
        // Reduce combinations of lexemes into new lexemes
        for (j = 0; j < lexeme.length; ++j) {
          lexHead = lexResult.pop();
          lexReduction = reduce(lexHead, lexeme[j]);
          lexResult = lexResult.concat(lexReduction);
        }
        // Parse (stream) lexical tokens into a syntax tree
        while (lexResult.length > 1)
          parse(lexResult.shift());
      }
      if (lexResult.length == 1)
        parse(lexResult.shift());
      return { ast: astResult };
    };

  })();