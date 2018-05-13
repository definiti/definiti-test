package definiti.tests.validation.controls.expression

import definiti.common.ast.{Library, Location, NativeClassDefinition}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.ast.{MethodCall, Type}
import definiti.tests.validation.ValidationContext
import definiti.tests.validation.helpers.Types

object MethodExistenceControl extends Control[ValidationContext] {
  override def description: String = "Checks if the method exists for a method call"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: ValidationContext, library: Library): ControlResult = {
    context.extractExpressions {
      case methodCall: MethodCall => methodCall
    }
      .map(controlMethodCall(_, context))
  }

  private def controlMethodCall(methodCall: MethodCall, context: ValidationContext): ControlResult = {
    val innerType = Types.getTypeOfExpression(methodCall.inner, context)
    context.getFinalClassDefinition(innerType.name) match {
      case Some(native: NativeClassDefinition) if native.methods.exists(_.name == methodCall.method) =>
        ControlResult.OK
      case _ =>
        unknownMethod(innerType, methodCall.method, methodCall.location)
    }
  }

  def unknownMethod(typ: Type, method: String, location: Location): Alert = {
    alert(s"The type ${typ.readableString} does not contain the method ${method}", location)
  }
}
