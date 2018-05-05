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

testSubCase: expression ('with' arguments)?;

expression
  : BOOLEAN
  | NUMBER
  | STRING
  ;

arguments
  : '(' ')'
  | '(' (expression ',')* expression ')'
  ;

DOC_COMMENT   : '/**' .*? '*/';
BLOCK_COMMENT : '/*' .*? '*/' -> skip;
LINE_COMMENT  : '//' ~[\r\n]* -> skip;

WS : [ \r\n\t]+ -> skip;