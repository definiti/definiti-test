package definiti.tests.validation.controls.expression

import definiti.common.ast.{Library, Location}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.ast.Reference
import definiti.tests.validation.ValidationContext
import definiti.tests.validation.helpers.ScopedExpression

object ReferenceControl extends Control[ValidationContext] {
  override def description: String = "Check if the reference is valid"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: ValidationContext, library: Library): ControlResult = {
    context.extractExpressions {
      case reference: Reference => reference
    }
      .map(controlReference)
  }

  private def controlReference(scopedExpression: ScopedExpression[Reference]): ControlResult = {
    val referenceExistsInScope = scopedExpression.references.contains(scopedExpression.target)
    val referencExistsAsEnum = scopedExpression.hasEnum(scopedExpression.target)
    if (referenceExistsInScope || referencExistsAsEnum) {
      ControlResult.OK
    } else {
      unknownReference(scopedExpression.target, scopedExpression.location)
    }
  }

  def unknownReference(reference: String, location: Location): Alert = {
    alert(s"The reference ${reference} does not exist", location)
  }
}
