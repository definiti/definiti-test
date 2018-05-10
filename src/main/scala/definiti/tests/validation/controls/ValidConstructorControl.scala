package definiti.tests.validation.controls

import definiti.common.ast.{Library, Location}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.AST._
import definiti.tests.validation.helpers.ExpressionTypes

object ValidConstructorControl extends Control[TestsContext] {
  override def description: String = "Check if type constructors are valid"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: TestsContext, library: Library): ControlResult = {
    extractConstructorExpressions(context)
      .map(controlConstructorExpression)
  }

  private def extractConstructorExpressions(context: TestsContext): Seq[ConstructorExpression] = {
    (context.testVerifications.flatMap(_.cases) ++ context.testTypes.flatMap(_.cases))
      .flatMap(_.subCases)
      .flatMap { subCase =>
        (subCase.expression +: subCase.arguments) ++ subCase.messageArguments
      }
      .flatMap(extractConstructorsFromExpression)
  }

  private def extractConstructorsFromExpression(expression: Expression): Seq[ConstructorExpression] = {
    expression match {
      case constructorExpression: ConstructorExpression =>
        constructorExpression +: constructorExpression.arguments.flatMap(extractConstructorsFromExpression)
      case structureExpression: StructureExpression =>
        structureExpression.fields.map(_.expression).flatMap(extractConstructorsFromExpression)
      case _ =>
        Seq.empty
    }
  }

  private def controlConstructorExpression(expression: ConstructorExpression): ControlResult = {
    val typ = ExpressionTypes.getTypeOfExpression(expression)
    typ.name match {
      case "List" =>
        typ.generics.headOption match {
          case Some(generic) => ControlResult.squash(expression.arguments.map(controlInnerTypeOfExpression(_, generic)))
          case None => ignored
        }
      case "Option" =>
        if (expression.arguments.length <= 1) {
          typ.generics.headOption match {
            case Some(generic) => ControlResult.squash(expression.arguments.map(controlInnerTypeOfExpression(_, generic)))
            case None => ignored
          }
        } else {
          invalidNumberOfArgument(Seq(0, 1), expression.arguments.length, expression.location)
        }
      case _ =>
        ControlResult.OK
    }
  }

  private def controlInnerTypeOfExpression(expression: Expression, expectedType: Type): ControlResult = {
    val typ = ExpressionTypes.getTypeOfExpression(expression)
    if (typ == expectedType) {
      ControlResult.OK
    } else {
      unexpectedType(expectedType, typ, expression.location)
    }
  }

  def invalidNumberOfArgument(expected: Seq[Int], got: Int, location: Location): Alert = {
    alert(s"Unexpected number of arguments (expected: ${expected.mkString(" or ")}, got: ${got})", location)
  }

  def unexpectedType(expected: Type, got: Type, location: Location): Alert = {
    alert(s"Unexpected type (expected: ${expected.readableString}, got: ${got.readableString})", location)
  }
}
