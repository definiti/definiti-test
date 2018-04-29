package definiti.tests.validation

import definiti.tests.AST.{TestVerification, TestsContext}

package object controls {
  implicit class TestsContextExtension(testsContext: TestsContext) {
    def testVerifications: Seq[TestVerification] = {
      testsContext.tests
        .collect {
          case testVerification: TestVerification => testVerification
        }
    }
  }
}
