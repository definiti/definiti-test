package definiti.tests.validation.controls

import definiti.common.ast.{Library, Location}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.AST.{Generator, Type}
import definiti.tests.validation.ValidationContext

object TypeReferenceForGeneratorControl extends Control[ValidationContext] {
  override def description: String = "Control if the type reference of the generator target a known type"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: ValidationContext, library: Library): ControlResult = {
    context.context.generators.map { generator =>
      controlType(generator.typ, generator, context)
    }
  }

  private def controlType(typ: Type, generator: Generator, context: ValidationContext): ControlResult = {
    if (generator.generics.contains(typ.name) || context.hasType(typ.name)) {
      typ.generics.map(controlType(_, generator, context))
    } else {
      unknownReference(typ, generator.location)
    }
  }

  def unknownReference(typ: Type, location: Location): Alert = {
    alert(s"The type ${typ.readableString} does not exist", location)
  }
}
