package definiti.tests.validation.controls.verificationTest

import definiti.common.ast.{Library, Location}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.ast._
import definiti.tests.validation.ValidationContext

object VerificationMessageArgumentsOnlyForRefusedCaseControl extends Control[ValidationContext] {
  override def description: String = "Control if message arguments are given only for 'accept' cases"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: ValidationContext, library: Library): ControlResult = {
    context.testVerifications
      .flatMap(_.cases)
      .filter(_.kind == CaseKind.accept)
      .flatMap(_.subCases)
      .map(controlSubCaseOnAccept)
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
