package definiti.tests.validation

import definiti.tests.AST.{TestType, TestVerification, TestsContext}

package object controls {
  implicit class TestsContextExtension(testsContext: TestsContext) {
    def testVerifications: Seq[TestVerification] = {
      testsContext.tests
        .collect {
          case testVerification: TestVerification => testVerification
        }
    }

    def testTypes: Seq[TestType] = {
      testsContext.tests
        .collect {
          case testType: TestType => testType
        }
    }
  }
}
