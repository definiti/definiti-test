package definiti.tests.validation.controls

import definiti.common.ast.{Library, Location}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.AST
import definiti.tests.AST.{Expression, TestVerification, TestsContext, Type}
import definiti.tests.validation.helpers.ExpressionTypes

object ValidExpressionTypeControl extends Control[TestsContext] {
  override def description: String = "Check if an expression returns a known type"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: AST.TestsContext, library: Library): ControlResult = {
    extractExpressions(context)
      .map(controlExpression(_, library))
  }

  private def extractExpressions(context: AST.TestsContext): Seq[Expression] = {
    (context.testVerifications.flatMap(_.cases) ++ context.testTypes.flatMap(_.cases))
      .flatMap(_.subCases)
      .flatMap { subCase =>
        (subCase.expression +: subCase.arguments) ++ subCase.messageArguments
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
