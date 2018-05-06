grammar Tests;

BOOLEAN      : 'true' | 'false';
NUMBER       : [0-9]+('.'[0-9]+)?;
STRING       : '"' ( '\\"' | . )*? '"';
TEST         : 'test';
VERIFICATION : 'verification';
ACCEPT       : 'accept';
REFUSE       : 'refuse';
IDENTIFIER   : [a-zA-Z0-9]+;

tests: toplevel*;

toplevel
  : testVerification
  ;

testVerification:
  DOC_COMMENT?
  TEST VERIFICATION IDENTIFIER '{'
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
  ;

constructor: type arguments;

arguments
  : '(' ')'
  | '(' (expression ',')* expression ')'
  ;

generics: '[' (type ',')* type ']';
type: IDENTIFIER generics?;

DOC_COMMENT   : '/**' .*? '*/';
BLOCK_COMMENT : '/*' .*? '*/' -> skip;
LINE_COMMENT  : '//' ~[\r\n]* -> skip;

WS : [ \r\n\t]+ -> skip;