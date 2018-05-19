package definiti.tests.validation.controls.typeTest

import definiti.common.ast.{Library, Location}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.ast._
import definiti.tests.validation.ValidationContext
import definiti.tests.validation.helpers.{ScopedExpression, Types}

object InputTypeForTypeTestControl extends Control[ValidationContext] {
  override def description: String = "Control if given input is the same as type input"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: ValidationContext, library: Library): ControlResult = {
    extractExpressions(context)
      .map { case (typ, expression) => controlExpression(expression, typ, context) }
  }

  private def extractExpressions(context: ValidationContext): Seq[(Type, ScopedExpression[Expression])] = {
    for {
      test <- context.testTypes
      testCase <- test.cases
      subCase <- testCase.subCases
    } yield {
      test.typ -> ScopedExpression(subCase.expression, context)
    }
  }

  private def controlExpression(expression: ScopedExpression[Expression], typ: Type, context: ValidationContext): ControlResult = {
    if (Types.finalType(typ, context) == expression.typeOfExpression) {
      ControlResult.OK
    } else {
      invalidType(typ, expression.location)
    }
  }

  def invalidType(expectedType: Type, location: Location): Alert = {
    alert(s"The expression does not match the expected type ${expectedType.readableString}", location)
  }
}
