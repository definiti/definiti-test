package definiti.tests.end2end.controls.typeTest

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.LocationPath
import definiti.tests.ConfigurationBuilder
import definiti.tests.ast.Type
import definiti.tests.end2end.EndToEndSpec
import definiti.tests.validation.controls.typeTest.TypeReferenceForTypeTestControl

class TypeReferenceForTypeTestControlSpec extends EndToEndSpec {
  import TypeReferenceForTypeTestControlSpec._

  "TestsValidation" should "validate a reference to a type" in {
    val output = processFile("controls.typeTest.typeReferenceForTypeTest.validReference", configuration)
    output shouldBe ok[Root]
  }

  it should "validate a reference to a type when a package in provided" in {
    val output = processFile("controls.typeTest.typeReferenceForTypeTest.validReferenceInPackage", configuration)
    output shouldBe ok[Root]
  }

  it should "validate a reference to a type with valid generics" in {
    val output = processFile("controls.typeTest.typeReferenceForTypeTest.validReferenceInPackage", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a reference to a type when the type does not exist" in {
    val output = processFile("controls.typeTest.typeReferenceForTypeTest.invalidReference", configuration)
    output should beResult(Ko[Root](
      TypeReferenceForTypeTestControl.unknownReference(Type("Unknown"), invalidReferenceLocation(2, 1, 21))
    ))
  }

  it should "invalidate a reference to a type when one of its generics does not exist" in {
    val output = processFile("controls.typeTest.typeReferenceForTypeTest.invalidGenericReference", configuration)
    output should beResult(Ko[Root](
      TypeReferenceForTypeTestControl.unknownReference(Type("Person"), invalidGenericReferenceLocation(6, 1, 30))
    ))
  }
}

object TypeReferenceForTypeTestControlSpec {
  val configuration = ConfigurationBuilder().withOnlyControls(TypeReferenceForTypeTestControl).build()

  val controlDirectory = s"typeTest/${TypeReferenceForTypeTestControl.name}"
  val invalidReferenceLocation = LocationPath.control(controlDirectory, "invalidReference")
  val invalidGenericReferenceLocation = LocationPath.control(controlDirectory, "invalidGenericReference")
}
