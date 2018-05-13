package definiti.tests.end2end.controls

import definiti.common.ast.{Root, TypeReference}
import definiti.common.program.Ko
import definiti.common.tests.LocationPath
import definiti.tests.ast.Type
import definiti.tests.ConfigurationBuilder
import definiti.tests.end2end.EndToEndSpec
import definiti.tests.validation.controls.StructureControl

class StructureControlSpec extends EndToEndSpec {
  import StructureControlSpec._

  "TestsValidation" should "validate valid structure for verification" in {
    val output = processFile("controls.structure.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "validate valid structure for type" in {
    val output = processFile("controls.structure.nominalForType", configuration)
    output shouldBe ok[Root]
  }

  it should "validate valid structure for generator" in {
    val output = processFile("controls.structure.nominalForGenerator", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a structure missing a field for verification" in {
    val output = processFile("controls.structure.fieldMissing", configuration)
    output should beResult(Ko[Root](
      StructureControl.fieldMissing("lastName", Type("x.Person"), fieldMissingLocation(17, 12, 19, 6)),
      StructureControl.fieldMissing("firstName", Type("x.Person"), fieldMissingLocation(21, 12, 23, 6))
    ))
  }

  it should "invalidate a structure missing a field for types" in {
    val output = processFile("controls.structure.fieldMissingForType", configuration)
    output should beResult(Ko[Root](
      StructureControl.fieldMissing("lastName", Type("x.Person"), fieldMissingForTypeLocation(10, 12, 12, 6)),
      StructureControl.fieldMissing("firstName", Type("x.Person"), fieldMissingForTypeLocation(14, 12, 16, 6))
    ))
  }

  it should "invalidate a structure missing a field for generators" in {
    val output = processFile("controls.structure.fieldMissingForGenerator", configuration)
    output should beResult(Ko[Root](
      StructureControl.fieldMissing("lastName", Type("x.Person"), fieldMissingForGeneratorLocation(9, 33, 11, 4)),
      StructureControl.fieldMissing("firstName", Type("x.Person"), fieldMissingForGeneratorLocation(13, 40, 15, 4))
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

  it should "validate a structure with a nested structure" in {
    val output = processFile("controls.structure.nominalDeepStructure", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a structure with an invalid nested structure" in {
    val output = processFile("controls.structure.invalidNestedType", configuration)
    output should beResult(Ko[Root](
      StructureControl.invalidType(TypeReference("x.Contact"), Type("Boolean"), invalidNestedTypeLocation(26, 16, 20)),
      StructureControl.fieldMissing("phone", Type("x.Contact"), invalidNestedTypeLocation(32, 16, 34, 8))
    ))
  }

  it should "invalidate a structure with an invalid nested structure in List" in {
    val output = processFile("controls.structure.invalidNestedTypeInList", configuration)
    output should beResult(Ko[Root](
      StructureControl.invalidType(TypeReference("List", Seq(TypeReference("x.Contact"))), Type("List", Type("Boolean")), invalidNestedTypeInListLocation(26, 17, 36)),
      StructureControl.fieldMissing("phone", Type("x.Contact"), invalidNestedTypeInListLocation(32, 31, 34, 8))
    ))
  }
}

object StructureControlSpec {
  val configuration = ConfigurationBuilder().withOnlyControls(StructureControl).build()

  val fieldMissingLocation = LocationPath.control(StructureControl, "fieldMissing")
  val fieldMissingForTypeLocation = LocationPath.control(StructureControl, "fieldMissingForType")
  val fieldMissingForGeneratorLocation = LocationPath.control(StructureControl, "fieldMissingForGenerator")
  val fieldAddedLocation = LocationPath.control(StructureControl, "fieldAdded")
  val invalidFieldTypeLocation = LocationPath.control(StructureControl, "invalidFieldType")
  val invalidNestedTypeLocation = LocationPath.control(StructureControl, "invalidNestedType")
  val invalidNestedTypeInListLocation = LocationPath.control(StructureControl, "invalidNestedTypeInList")
}