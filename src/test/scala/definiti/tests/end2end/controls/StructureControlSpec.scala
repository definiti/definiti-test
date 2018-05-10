package definiti.tests.end2end.controls

import definiti.common.ast.{Root, TypeReference}
import definiti.common.program.Ko
import definiti.common.tests.LocationPath
import definiti.tests.AST.Type
import definiti.tests.ConfigurationBuilder
import definiti.tests.end2end.EndToEndSpec
import definiti.tests.validation.controls.StructureControl

class StructureControlSpec extends EndToEndSpec {
  import StructureControlSpec._

  "TestsValidation" should "validate valid structure" in {
    val output = processFile("controls.structure.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a structure missing a field" in {
    val output = processFile("controls.structure.fieldMissing", configuration)
    output should beResult(Ko[Root](
      StructureControl.fieldMissing("lastName", Type("x.Person"), fieldMissingLocation(17, 12, 19, 6)),
      StructureControl.fieldMissing("firstName", Type("x.Person"), fieldMissingLocation(21, 12, 23, 6))
    ))
  }

  it should "invalidate a structure with another field" in {
    val output = processFile("controls.structure.fieldAdded", configuration)
    output should beResult(Ko[Root](
      StructureControl.fieldAdded("another", Type("x.Person"), fieldAddedLocation(20, 7, 25)),
      StructureControl.fieldAdded("undefined", Type("x.Person"), fieldAddedLocation(26, 7, 29))
    ))
  }

  it should "invalidate a structure with a field with an invalid value" in {
    val output = processFile("controls.structure.invalidFieldType", configuration)
    output should beResult(Ko[Root](
      StructureControl.invalidType(TypeReference("String"), Type("Number"), invalidFieldTypeLocation(18, 18, 19)),
      StructureControl.invalidType(TypeReference("String"), Type("Boolean"), invalidFieldTypeLocation(19, 17, 21))
    ))
  }
}

object StructureControlSpec {
  val configuration = ConfigurationBuilder().withOnlyControls(StructureControl).build()

  val fieldMissingLocation = LocationPath.control(StructureControl, "fieldMissing")
  val fieldAddedLocation = LocationPath.control(StructureControl, "fieldAdded")
  val invalidFieldTypeLocation = LocationPath.control(StructureControl, "invalidFieldType")
}