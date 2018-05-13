package definiti.tests.validation.controls.expression

import definiti.common.ast.{Expression => _, MethodCall => _, _}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.ast._
import definiti.tests.validation.ValidationContext
import definiti.tests.validation.helpers.{ScopedType, Types}

object MethodArgumentsControl extends Control[ValidationContext] {
  override def description: String = "Checks if arguments of the method are the same as the definition"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: ValidationContext, library: Library): ControlResult = {
    context.extractExpressions {
      case methodCall: MethodCall => methodCall
    }
      .map(controlMethodCall(_, context))
  }

  private def controlMethodCall(methodCall: MethodCall, context: ValidationContext): ControlResult = {
    getMethod(methodCall, context)
      .map { methodDefinition =>
        if (methodDefinition.parameters.length == methodCall.arguments.length) {
          controlParametersAndArguments(methodCall, methodDefinition, context)
        } else {
          ControlResult(invalidNumberOfArguments(methodDefinition.parameters.length, methodCall.arguments.length, methodCall.location))
        }
      }
      .getOrElse(ignored)
  }

  private def controlParametersAndArguments(methodCall: MethodCall, methodDefinition: MethodDefinition, context: ValidationContext): ControlResult = {
    methodDefinition.parameters.zip(methodCall.arguments)
      .map { case (parameter, argument) =>
        val typeOfArgument = Types.finalType(Types.getTypeOfExpression(argument, context), context)
        val scopedTypeOfParameter = ScopedType(parameter.typeReference, methodDefinition)
        if (scopedTypeOfParameter.isSameAs(typeOfArgument)) {
          ControlResult.OK
        } else {
          ControlResult(invalidTypeOfArgument(parameter.typeReference, typeOfArgument, argument.location))
        }
      }
  }

  private def getMethod(methodCall: MethodCall, context: ValidationContext): Option[MethodDefinition] = {
    val innerType = Types.getTypeOfExpression(methodCall.inner, context)
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
