package definiti.tests.validation.controls

import definiti.common.ast.{Library, Location}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.AST._
import definiti.tests.validation.ValidationContext

object VerificationReferenceForVerificationTestControl extends Control[ValidationContext] {
  override def description: String = "Control if the verification reference of the test target a known verification"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: ValidationContext, library: Library): ControlResult = {
    context.testVerifications.map(controlTestVerification(_, context))
  }

  private def controlTestVerification(testVerification: TestVerification, context: ValidationContext): ControlResult = {
    if (context.hasVerification(testVerification.verification)) {
      ControlResult.OK
    } else {
      unknownReference(testVerification.verification, testVerification.location)
    }
  }

  def unknownReference(verificationName: String, location: Location): Alert = {
    alert(s"The verification ${verificationName} does not exist", location)
  }
}
