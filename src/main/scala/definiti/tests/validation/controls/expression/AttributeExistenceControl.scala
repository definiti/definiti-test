package definiti.tests.validation.controls.expression

import definiti.common.ast.{DefinedType, Enum, Library, Location, NativeClassDefinition}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.ast.{AttributeCall, MethodCall, Type}
import definiti.tests.validation.ValidationContext
import definiti.tests.validation.helpers.Types

object AttributeExistenceControl extends Control[ValidationContext] {
  override def description: String = "Checks if the attribute exists for an attribute call"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: ValidationContext, library: Library): ControlResult = {
    context.extractExpressions {
      case attributeCall: AttributeCall => attributeCall
    }
      .map(controlAttributeCall(_, context))
  }

  private def controlAttributeCall(attributeCall: AttributeCall, context: ValidationContext): ControlResult = {
    val innerType = Types.getTypeOfExpression(attributeCall.inner, context)
    val classOfInner = context.getFinalClassDefinition(innerType.name)
    classOfInner match {
      case Some(native: NativeClassDefinition) if native.attributes.exists(_.name == attributeCall.attribute) =>
        ControlResult.OK
      case Some(definedType: DefinedType) if definedType.attributes.exists(_.name == attributeCall.attribute) =>
        ControlResult.OK
      case Some(enum: Enum) if enum.cases.exists(_.name == attributeCall.attribute) =>
        ControlResult.OK
      case _ =>
        unknownAttribute(innerType, attributeCall.attribute, attributeCall.location)
    }
  }

  def unknownAttribute(typ: Type, attribute: String, location: Location): Alert = {
    alert(s"The type ${typ.readableString} does not contain the attribute ${attribute}", location)
  }
}
