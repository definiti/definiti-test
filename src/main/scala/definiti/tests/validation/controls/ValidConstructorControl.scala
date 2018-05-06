package definiti.tests.validation.controls

import definiti.core.Alert
import definiti.core.ast.{Library, Location}
import definiti.core.validation.{ControlLevel, ControlResult}
import definiti.tests.AST
import definiti.tests.AST.{ConstructorExpression, Expression, TestVerification, Type}
import definiti.tests.validation.Control
import definiti.tests.validation.helpers.ExpressionTypes

object ValidConstructorControl extends Control {
  override def description: String = "Check if type constructors are valid"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: AST.TestsContext, library: Library): ControlResult = {
    extractConstructorExpressions(context)
      .map(controlConstructorExpression)
  }

  private def extractConstructorExpressions(context: AST.TestsContext): Seq[ConstructorExpression] = {
    context.tests
      .flatMap {
        case test: TestVerification =>
          test.cases
            .flatMap(_.subCases)
            .flatMap { subCase =>
              (subCase.expression +: subCase.arguments) ++ subCase.messageArguments
            }
        case _ => Seq.empty
      }
      .collect {
        case constructorExpression: ConstructorExpression => constructorExpression
      }
  }

  private def controlConstructorExpression(expression: ConstructorExpression): ControlResult = {
    val typ = ExpressionTypes.getTypeOfExpression(expression)
    typ.name match {
      case "List" =>
        typ.generics.headOption match {
          case Some(generic) => expression.arguments.map(controlInnerTypeOfExpression(_, generic))
          case None => ignored
        }
      case "Option" =>
        if (expression.arguments.length <= 1) {
          typ.generics.headOption match {
            case Some(generic) => expression.arguments.map(controlInnerTypeOfExpression(_, generic))
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
      expression match {
        case constructorExpression: ConstructorExpression => controlConstructorExpression(constructorExpression)
        case _ => ControlResult.OK
      }
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
