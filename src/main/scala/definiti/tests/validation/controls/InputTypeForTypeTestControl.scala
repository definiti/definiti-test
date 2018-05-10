package definiti.tests.validation.controls

import definiti.common.ast.{Library, Location}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.AST._
import definiti.tests.validation.helpers.ExpressionTypes

object InputTypeForTypeTestControl extends Control[TestsContext] {
  override def description: String = "Control if given input is the same as type input"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: TestsContext, library: Library): ControlResult = {
    context.testTypes.map(controlTestType)
  }

  private def controlTestType(testType: TestType): ControlResult = {
    testType.cases.map(controlTestCase(_, testType.typ))
  }

  private def controlTestCase(testCase: Case, typ: Type): ControlResult = {
    ControlResult.squash {
      testCase.subCases.map(subCase => controlExpression(subCase.expression, typ))
    }
  }

  private def controlExpression(expression: Expression, typ: Type): ControlResult = {
    if (typ == ExpressionTypes.getTypeOfExpression(expression)) {
      ControlResult.OK
    } else {
      invalidType(typ, expression.location)
    }
  }

  def invalidType(expectedType: Type, location: Location): Alert = {
    alert(s"The expression does not match the expected type ${expectedType.readableString}", location)
  }
}
