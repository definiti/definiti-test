package definiti.tests.validation.controls.expression

import definiti.common.ast.{Expression => _, MethodCall => _, _}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.ast._
import definiti.tests.validation.ValidationContext
import definiti.tests.validation.helpers.{ScopedExpression, ScopedType, Types}

object MethodArgumentsControl extends Control[ValidationContext] {
  override def description: String = "Checks if arguments of the method are the same as the definition"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: ValidationContext, library: Library): ControlResult = {
    context.extractExpressions {
      case methodCall: MethodCall => methodCall
    }
      .map(controlMethodCall(_, context))
  }

  private def controlMethodCall(scopedMethodCall: ScopedExpression[MethodCall], context: ValidationContext): ControlResult = {
    getMethod(scopedMethodCall, context)
      .map { methodDefinition =>
        if (methodDefinition.parameters.length == scopedMethodCall.arguments.length) {
          controlParametersAndArguments(scopedMethodCall, methodDefinition, context)
        } else {
          ControlResult(invalidNumberOfArguments(methodDefinition.parameters.length, scopedMethodCall.arguments.length, scopedMethodCall.location))
        }
      }
      .getOrElse(ignored)
  }

  private def controlParametersAndArguments(methodCall: ScopedExpression[MethodCall], methodDefinition: MethodDefinition, context: ValidationContext): ControlResult = {
    methodDefinition.parameters.zip(methodCall.arguments)
      .map { case (parameter, argument) =>
        val typeOfArgument = Types.finalType(argument.typeOfExpression, context)
        val scopedTypeOfParameter = ScopedType(parameter.typeReference, methodDefinition, context)
        if (scopedTypeOfParameter.isSameAs(typeOfArgument)) {
          ControlResult.OK
        } else {
          ControlResult(invalidTypeOfArgument(parameter.typeReference, typeOfArgument, argument.location))
        }
      }
  }

  private def getMethod(methodCall: ScopedExpression[MethodCall], context: ValidationContext): Option[MethodDefinition] = {
    val innerType = methodCall.inner.typeOfExpression
    context.getFinalClassDefinition(innerType.name)
      .collect { case native: NativeClassDefinition => native }
      .flatMap(_.methods.find(_.name == methodCall.method))
  }

  def invalidNumberOfArguments(expected: Int, got: Int, location: Location): Alert = {
    alert(s"Invalid number of arguments (expected: ${expected}, got: ${got})", location)
  }

  def invalidTypeOfArgument(expected: AbstractTypeReference, got: Type, location: Location): Alert = {
    alert(s"Invalid type of argument (expected: ${expected.readableString}, got: ${got.readableString})", location)
  }
}
