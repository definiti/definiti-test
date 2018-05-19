package definiti.tests.validation.controls.expression

import definiti.common.ast.{Library, Location}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.ast.{Condition, Type}
import definiti.tests.validation.ValidationContext
import definiti.tests.validation.helpers.ScopedExpression

object ConditionControl extends Control[ValidationContext] {
  override def description: String = "Check the condition is a boolean"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: ValidationContext, library: Library): ControlResult = {
    context.extractExpressions {
      case condition: Condition => condition
    }
      .map(controlCondition)
  }

  private def controlCondition(condition: ScopedExpression[Condition]): ControlResult = {
    if (condition.condition.typeOfExpression == Type("Boolean", Seq.empty)) {
      ControlResult.OK
    } else {
      invalidBoolean(condition.condition.typeOfExpression, condition.condition.location)
    }
  }

  def invalidBoolean(got: Type, location: Location): Alert = {
    alert(s"Invalid type (expected: Boolean, got: ${got.readableString})", location)
  }
}
