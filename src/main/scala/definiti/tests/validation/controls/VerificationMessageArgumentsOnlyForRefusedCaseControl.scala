package definiti.tests.validation.controls

import definiti.core.Alert
import definiti.core.ast.{AbstractTypeReference, Library, Location, Verification}
import definiti.core.validation.{ControlLevel, ControlResult}
import definiti.tests.AST
import definiti.tests.AST._
import definiti.tests.validation.Control
import definiti.tests.validation.controls.SubCaseVerificationReferenceTypesControl.{alert, ignored}
import definiti.tests.validation.helpers.ExpressionTypes

object VerificationMessageArgumentsOnlyForRefusedCaseControl extends Control {
  override def description: String = "Control if message arguments are given only for 'accept' cases"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: AST.TestsContext, library: Library): ControlResult = {
    ControlResult.squash {
      context.testVerifications.map(controlTestVerification(_, context, library))
    }
  }

  private def controlTestVerification(testVerification: TestVerification, context: TestsContext, library: Library): ControlResult = {
    ControlResult.squash {
      testVerification.cases.map(controlTestCase)
    }
  }

  private def controlTestCase(testCase: Case): ControlResult = {
    if (testCase.kind == CaseKind.refuse) {
      ControlResult.OK
    } else {
      ControlResult.squash {
        testCase.subCases.map(controlSubCaseOnAccept)
      }
    }
  }

  private def controlSubCaseOnAccept(subCase: SubCase): ControlResult = {
    if (subCase.messageArguments.isEmpty) {
      ControlResult.OK
    } else {
      unacceptedMessageArguments(subCase.location)
    }
  }

  def unacceptedMessageArguments(location: Location): Alert = {
    alert(s"Message arguments ('as' part) are not accepted for 'accept' cases", location)
  }
}
