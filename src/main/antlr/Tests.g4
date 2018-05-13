grammar Tests;

BOOLEAN      : 'true' | 'false';
NUMBER       : [0-9]+('.'[0-9]+)?;
STRING       : '"' ( '\\"' | . )*? '"';
TEST         : 'test';
VERIFICATION : 'verification';
TYPE         : 'type';
ACCEPT       : 'accept';
REFUSE       : 'refuse';
GENERATOR    : 'generator';
IDENTIFIER   : [a-zA-Z0-9]+;

tests: toplevel*;

toplevel
  : testVerification
  | testType
  | generator
  ;

testVerification:
  DOC_COMMENT?
  TEST VERIFICATION IDENTIFIER '{'
    testCase*
  '}'
;

testType:
  DOC_COMMENT?
  TEST TYPE type '{'
    testCase*
  '}'
;

generator: GENERATOR name=IDENTIFIER rawGenerics? parameters ':' type '=' expression;

testCase:
  DOC_COMMENT?
  kind=(ACCEPT | REFUSE) testSubCase+;

testSubCase: expression ('with' withArguments=arguments)? ('as' asArguments=arguments)?;

expression
  : BOOLEAN
  | NUMBER
  | STRING
  | generation
  | structure
  | inner=expression '.' method=IDENTIFIER generics? arguments // method call
  | inner=expression '.' attribute=IDENTIFIER                  // attribute call
  | reference=IDENTIFIER
  ;

generation: name=IDENTIFIER generics? arguments;

arguments
  : '(' ')'
  | '(' (expression ',')* expression ')'
  ;

parameters
  : '(' ')'
  | '(' (parameter ',')* parameter ')'
  ;
parameter: IDENTIFIER ':' type;

generics: '[' (type ',')* type ']';
type: IDENTIFIER generics?;

rawGenerics: '[' (IDENTIFIER ',')* IDENTIFIER ']';

structure: type '{' field* '}';
field: IDENTIFIER ':' expression;

DOC_COMMENT   : '/**' .*? '*/';
BLOCK_COMMENT : '/*' .*? '*/' -> skip;
LINE_COMMENT  : '//' ~[\r\n]* -> skip;

WS : [ \r\n\t]+ -> skip;