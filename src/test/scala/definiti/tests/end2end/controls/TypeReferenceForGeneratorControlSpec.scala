package definiti.tests.end2end.controls

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.LocationPath
import definiti.tests.ast.Type
import definiti.tests.ConfigurationBuilder
import definiti.tests.end2end.EndToEndSpec
import definiti.tests.validation.controls.TypeReferenceForGeneratorControl

class TypeReferenceForGeneratorControlSpec extends EndToEndSpec {
  import TypeReferenceForGeneratorControlSpec._

  "TestsValidation" should "validate a reference to a type" in {
    val output = processFile("controls.typeReferenceForGenerator.validReference", configuration)
    output shouldBe ok[Root]
  }

  it should "validate a reference to a type when a package in provided" in {
    val output = processFile("controls.typeReferenceForGenerator.validReferenceInPackage", configuration)
    output shouldBe ok[Root]
  }

  it should "validate a reference to a type with valid generics" in {
    val output = processFile("controls.typeReferenceForGenerator.validReferenceInPackage", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a reference to a type when the type does not exist" in {
    val output = processFile("controls.typeReferenceForGenerator.invalidReference", configuration)
    output should beResult(Ko[Root](
      TypeReferenceForGeneratorControl.unknownReference(Type("Unknown"), invalidReferenceLocation(2, 1, 37))
    ))
  }

  it should "invalidate a reference to a type when one of its generics does not exist" in {
    val output = processFile("controls.typeReferenceForGenerator.invalidGenericReference", configuration)
    output should beResult(Ko[Root](
      TypeReferenceForGeneratorControl.unknownReference(Type("Person"), invalidGenericReferenceLocation(6, 1, 8, 4))
    ))
  }
}

object TypeReferenceForGeneratorControlSpec {
  val configuration = ConfigurationBuilder().withOnlyControls(TypeReferenceForGeneratorControl).build()

  val invalidReferenceLocation = LocationPath.control(TypeReferenceForGeneratorControl, "invalidReference")
  val invalidGenericReferenceLocation = LocationPath.control(TypeReferenceForGeneratorControl, "invalidGenericReference")
}

