package definiti.tests.validation.controls

import definiti.common.ast.{Library, Location}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.AST._
import definiti.tests.validation.ValidationContext

object TypeReferenceForTypeTestControl extends Control[ValidationContext] {
  override def description: String = "Control if the type reference of the test target a known type"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: ValidationContext, library: Library): ControlResult = {
    context.testTypes.map { testType =>
      controlType(testType.typ, testType.location, context)
    }
  }

  private def controlType(typ: Type, location: Location, context: ValidationContext): ControlResult = {
    if (context.hasType(typ.name)) {
      typ.generics.map(controlType(_, location, context))
    } else {
      unknownReference(typ, location)
    }
  }

  def unknownReference(typ: Type, location: Location): Alert = {
    alert(s"The type ${typ.readableString} does not exist", location)
  }
}
