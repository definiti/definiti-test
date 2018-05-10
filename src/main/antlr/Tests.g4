grammar Tests;

BOOLEAN      : 'true' | 'false';
NUMBER       : [0-9]+('.'[0-9]+)?;
STRING       : '"' ( '\\"' | . )*? '"';
TEST         : 'test';
VERIFICATION : 'verification';
TYPE         : 'type';
ACCEPT       : 'accept';
REFUSE       : 'refuse';
IDENTIFIER   : [a-zA-Z0-9]+;

tests: toplevel*;

toplevel
  : testVerification
  | testType
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

testCase:
  DOC_COMMENT?
  kind=(ACCEPT | REFUSE) testSubCase+;

testSubCase: expression ('with' withArguments=arguments)? ('as' asArguments=arguments)?;

expression
  : BOOLEAN
  | NUMBER
  | STRING
  | constructor
  | structure
  ;

constructor: type arguments;

arguments
  : '(' ')'
  | '(' (expression ',')* expression ')'
  ;

generics: '[' (type ',')* type ']';
type: IDENTIFIER generics?;

structure: type '{' field* '}';
field: IDENTIFIER ':' expression;

DOC_COMMENT   : '/**' .*? '*/';
BLOCK_COMMENT : '/*' .*? '*/' -> skip;
LINE_COMMENT  : '//' ~[\r\n]* -> skip;

WS : [ \r\n\t]+ -> skip;