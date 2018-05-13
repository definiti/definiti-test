package definiti.tests.validation.controls

import definiti.common.ast.{Library, Location}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.AST._
import definiti.tests.validation.ValidationContext
import definiti.tests.validation.helpers.{ExpressionTypes, Types}

object InputTypeForTypeTestControl extends Control[ValidationContext] {
  override def description: String = "Control if given input is the same as type input"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: ValidationContext, library: Library): ControlResult = {
    extractExpressions(context)
      .map { case (typ, expression) => controlExpression(expression, typ, context) }
  }

  private def extractExpressions(context: ValidationContext): Seq[(Type, Expression)] = {
    for {
      test <- context.testTypes
      testCase <- test.cases
      subCase <- testCase.subCases
    } yield {
      test.typ -> subCase.expression
    }
  }

  private def controlExpression(expression: Expression, typ: Type, context: ValidationContext): ControlResult = {
    if (Types.finalType(typ, context) == ExpressionTypes.getTypeOfExpression(expression, context)) {
      ControlResult.OK
    } else {
      invalidType(typ, expression.location)
    }
  }

  def invalidType(expectedType: Type, location: Location): Alert = {
    alert(s"The expression does not match the expected type ${expectedType.readableString}", location)
  }
}
