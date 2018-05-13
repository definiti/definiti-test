package definiti.tests.validation.controls

import definiti.common.ast.{Library, Location}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.AST.{Generator, Type}
import definiti.tests.validation.ValidationContext
import definiti.tests.validation.helpers.ExpressionTypes

object ExpressionTypeOfGeneratorControl extends Control[ValidationContext] {
  override def description: String = "Control that the expression of generator matches its declaration"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: ValidationContext, library: Library): ControlResult = {
    context.context.generators.map(controlGenerator(_, context))
  }

  private def controlGenerator(generator: Generator, context: ValidationContext): ControlResult = {
    val expressionType = ExpressionTypes.getTypeOfExpression(generator.expression, context)
    if (expressionType == generator.typ) {
      ControlResult.OK
    } else {
      invalidType(generator.typ, expressionType, generator.expression.location)
    }
  }

  def invalidType(expected: Type, got: Type, location: Location): Alert = {
    alert(s"The type ${got.readableString} does not match expected type ${expected.readableString}", location)
  }
}
