package definiti.tests.validation.controls

import definiti.common.ast.{Library, Location}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.AST._
import definiti.tests.validation.helpers.{ExpressionTypes, Types}

object InputTypeForTypeTestControl extends Control[TestsContext] {
  override def description: String = "Control if given input is the same as type input"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: TestsContext, library: Library): ControlResult = {
    extractExpressions(context)
      .map { case (typ, expression) => controlExpression(expression, typ, library) }
  }

  private def extractExpressions(context: TestsContext): Seq[(Type, Expression)] = {
    for {
      test <- context.testTypes
      testCase <- test.cases
      subCase <- testCase.subCases
    } yield {
      test.typ -> subCase.expression
    }
  }

  private def controlExpression(expression: Expression, typ: Type, library: Library): ControlResult = {
    if (Types.finalType(typ, library) == ExpressionTypes.getTypeOfExpression(expression)) {
      ControlResult.OK
    } else {
      invalidType(typ, expression.location)
    }
  }

  def invalidType(expectedType: Type, location: Location): Alert = {
    alert(s"The expression does not match the expected type ${expectedType.readableString}", location)
  }
}
