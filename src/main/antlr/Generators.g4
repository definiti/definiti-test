grammar Generators;

BOOLEAN      : 'true' | 'false';
NUMBER       : [0-9]+('.'[0-9]+)?;
STRING       : '"' ( '\\"' | . )*? '"';
GENERATOR    : 'generator';
IDENTIFIER   : [a-zA-Z0-9]+;

generators: generator*;

generator: GENERATOR name=IDENTIFIER generics? parameters ':' type;

generics: '[' (IDENTIFIER ',')* IDENTIFIER ']';

parameters
  : '(' ')'
  | '(' (parameter ',')* parameter ')'
  ;

parameter: IDENTIFIER ':' type (genSymbol = '?')? (restSymbol = '*')?;

type: IDENTIFIER genericTypes?;
genericTypes: '[' (type ',')* type ']';

DOC_COMMENT   : '/**' .*? '*/';
BLOCK_COMMENT : '/*' .*? '*/' -> skip;
LINE_COMMENT  : '//' ~[\r\n]* -> skip;

WS : [ \r\n\t]+ -> skip;