package definiti.tests.validation.controls

import definiti.core.Alert
import definiti.core.ast.{Library, Location}
import definiti.core.validation.{ControlLevel, ControlResult}
import definiti.tests.AST
import definiti.tests.AST.{Expression, TestVerification, Type}
import definiti.tests.validation.Control
import definiti.tests.validation.helpers.ExpressionTypes

object ValidExpressionTypeControl extends Control {
  override def description: String = "Check if an expression returns a known type"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: AST.TestsContext, library: Library): ControlResult = {
    extractExpressions(context)
      .map(controlExpression(_, library))
  }

  private def extractExpressions(context: AST.TestsContext): Seq[Expression] = {
    context.tests.flatMap {
      case test: TestVerification =>
        test.cases
          .flatMap(_.subCases)
          .flatMap { subCase =>
            (subCase.expression +: subCase.arguments) ++ subCase.messageArguments
          }
      case _ => Seq.empty
    }
  }

  private def controlExpression(expression: Expression, library: Library): ControlResult = {
    val typ = ExpressionTypes.getTypeOfExpression(expression)
    if (isTypeValid(typ, library)) {
      ControlResult.OK
    } else {
      invalidType(typ, expression.location)
    }
  }

  private def isTypeValid(typ: Type, library: Library): Boolean = {
    library.typesMap.get(typ.name) match {
      case Some(classDefinition) =>
        if (typ.generics.length == classDefinition.genericTypes.length) {
          typ.generics.forall(isTypeValid(_, library))
        } else {
          false
        }
      case None => false
    }
  }

  def invalidType(typ: Type, location: Location): Alert = {
    alert(s"The type ${typ.readableString} or one of its generic types refers to any known type", location)
  }
}
