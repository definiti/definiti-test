package definiti.tests.validation.controls

import definiti.core.Alert
import definiti.core.ast.{Library, Location}
import definiti.core.validation.{ControlLevel, ControlResult}
import definiti.tests.AST._
import definiti.tests.validation.Control
import definiti.tests.validation.helpers.ExpressionTypes

object VerificationReferenceForVerificationTestControl extends Control {
  override def description: String = "Control if the verification reference of the test target a known verification"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: TestsContext, library: Library): ControlResult = {
    ControlResult.squash {
      context.testVerifications.map(controlTestVerification(_, context, library))
    }
  }

  private def controlTestVerification(testVerification: TestVerification, context: TestsContext, library: Library): ControlResult = {
    val verificationName = ExpressionTypes.fullVerificationName(testVerification.verification, context, library)
    if (library.verificationsMap.contains(verificationName)) {
      ControlResult.OK
    } else {
      unknownReference(testVerification.verification, testVerification.location)
    }
  }

  def unknownReference(verificationName: String, location: Location): Alert = {
    alert(s"The verification ${verificationName} does not exist", location)
  }
}
