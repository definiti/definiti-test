package definiti.tests.end2end.controls

import definiti.common.ast.{Root, TypeReference}
import definiti.common.program.Ko
import definiti.common.tests.LocationPath
import definiti.tests.AST.Type
import definiti.tests.ConfigurationBuilder
import definiti.tests.end2end.EndToEndSpec
import definiti.tests.validation.controls.InputTypeForTypeTestControl

class InputTypeForTypeTestControlSpec extends EndToEndSpec {
  import InputTypeForTypeTestControlSpec._

  "TestsValidation" should "validate valid type parameter" in {
    val output = processFile("controls.inputTypeForTypeTest.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "validate valid multiple expressions" in {
    val output = processFile("controls.inputTypeForTypeTest.validMultipleExpressions", configuration)
    output shouldBe ok[Root]
  }

  it should "validate valid type with generics" in {
    val output = processFile("controls.inputTypeForTypeTest.validGenerics", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a type when it does not match" in {
    val output = processFile("controls.inputTypeForTypeTest.invalidType", configuration)
    output should beResult(Ko[Root](
      InputTypeForTypeTestControl.invalidType(Type("Person"), invalidTypeLocation(11, 12, 13, 6)),
      InputTypeForTypeTestControl.invalidType(Type("Person"), invalidTypeLocation(14, 12, 14))
    ))
  }

  it should "invalidate a type when it does not match in multiple expression" in {
    val output = processFile("controls.inputTypeForTypeTest.invalidMultipleExpressions", configuration)
    output should beResult(Ko[Root](
      InputTypeForTypeTestControl.invalidType(Type("Person"), invalidMultipleExpressionsLocation(12, 7, 34)),
      InputTypeForTypeTestControl.invalidType(Type("Person"), invalidMultipleExpressionsLocation(13, 7, 9)),
      InputTypeForTypeTestControl.invalidType(Type("Person"), invalidMultipleExpressionsLocation(16, 7, 11)),
      InputTypeForTypeTestControl.invalidType(Type("Person"), invalidMultipleExpressionsLocation(17, 7, 19, 8))
    ))
  }

  it should "invalidate a type when generics do not match" in {
    val output = processFile("controls.inputTypeForTypeTest.invalidGenerics", configuration)
    output should beResult(Ko[Root](
      InputTypeForTypeTestControl.invalidType(Type("Container", Type("String")), invalidGenericsLocation(8, 7, 52)),
      InputTypeForTypeTestControl.invalidType(Type("Container", Type("String")), invalidGenericsLocation(11, 7, 13, 8))
    ))
  }
}

object InputTypeForTypeTestControlSpec {
  val configuration = ConfigurationBuilder().withOnlyControls(InputTypeForTypeTestControl).build()

  val invalidTypeLocation = LocationPath.control(InputTypeForTypeTestControl, "invalidType")
  val invalidMultipleExpressionsLocation = LocationPath.control(InputTypeForTypeTestControl, "invalidMultipleExpressions")
  val invalidGenericsLocation = LocationPath.control(InputTypeForTypeTestControl, "invalidGenerics")
}
