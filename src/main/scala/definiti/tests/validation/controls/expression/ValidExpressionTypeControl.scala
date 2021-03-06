package definiti.tests.validation.controls.expression

import definiti.common.ast.{Library, Location}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.ast.{Expression, Type}
import definiti.tests.validation.ValidationContext
import definiti.tests.validation.helpers.ScopedExpression

object ValidExpressionTypeControl extends Control[ValidationContext] {
  override def description: String = "Check if an expression returns a known type"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: ValidationContext, library: Library): ControlResult = {
    context.extractMainExpressions()
      .map(controlExpression(_, context))
  }

  private def controlExpression(expression: ScopedExpression[Expression], context: ValidationContext): ControlResult = {
    val typ = expression.typeOfExpression
    if (isTypeValid(typ, context, expression.definedGenerics)) {
      ControlResult.OK
    } else {
      invalidType(typ, expression.location)
    }
  }

  private def isTypeValid(typ: Type, context: ValidationContext, generics: Seq[String]): Boolean = {
    val isGeneric = generics.contains(typ.name)
    val isValidClass = context.getClassDefinition(typ.name)
      .exists { classDefinition =>
        typ.generics.length == classDefinition.genericTypes.length &&
          typ.generics.forall(isTypeValid(_, context, generics))
      }
    isGeneric || isValidClass
  }

  def invalidType(typ: Type, location: Location): Alert = {
    alert(s"The type ${typ.readableString} or one of its generic types refers to any known type", location)
  }
}
