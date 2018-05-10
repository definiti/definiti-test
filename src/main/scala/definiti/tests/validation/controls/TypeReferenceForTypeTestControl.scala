package definiti.tests.validation.controls

import definiti.common.ast.{Library, Location}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.AST._

object TypeReferenceForTypeTestControl extends Control[TestsContext] {
  override def description: String = "Control if the type reference of the test target a known type"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: TestsContext, library: Library): ControlResult = {
    context.testTypes.map(controlTestType(_, library))
  }

  private def controlTestType(testType: TestType, library: Library): ControlResult = {
    controlType(testType.typ, testType.location, library)
  }

  private def controlType(typ: Type, location: Location, library: Library): ControlResult = {
    if (library.typesMap.contains(typ.name)) {
      ControlResult.squash(typ.generics.map(controlType(_, location, library)))
    } else {
      unknownReference(typ, location)
    }
  }

  def unknownReference(typ: Type, location: Location): Alert = {
    alert(s"The type ${typ.readableString} does not exist", location)
  }
}
