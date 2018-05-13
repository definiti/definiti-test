package definiti.tests.end2end.controls.verificationTest

import definiti.common.ast.{Root, TypeReference}
import definiti.common.program.Ko
import definiti.common.tests.LocationPath
import definiti.tests.ConfigurationBuilder
import definiti.tests.end2end.EndToEndSpec
import definiti.tests.validation.controls.verificationTest.InputTypeForVerificationTestControl

class InputTypeForVerificationTestControlSpec extends EndToEndSpec {
  import InputTypeForVerificationTestControlSpec._

  "TestsValidation" should "validate valid string parameter" in {
    val output = processFile("controls.verificationTest.inputTypeForVerificationTest.validString", configuration)
    output shouldBe ok[Root]
  }

  it should "validate valid number parameter" in {
    val output = processFile("controls.verificationTest.inputTypeForVerificationTest.validNumber", configuration)
    output shouldBe ok[Root]
  }

  it should "validate valid multiple expressions" in {
    val output = processFile("controls.verificationTest.inputTypeForVerificationTest.validMultipleExpressions", configuration)
    output shouldBe ok[Root]
  }

  it should "validate valid list of type" in {
    val output = processFile("controls.verificationTest.inputTypeForVerificationTest.validListType", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a type when it does not match" in {
    val output = processFile("controls.verificationTest.inputTypeForVerificationTest.invalidType", configuration)
    output should beResult(Ko[Root](
      InputTypeForVerificationTestControl.invalidType(TypeReference("String"), invalidTypeLocation(12, 12, 13)),
      InputTypeForVerificationTestControl.invalidType(TypeReference("String"), invalidTypeLocation(13, 12, 17))
    ))
  }

  it should "invalidate a type when it does not match in multiple expression" in {
    val output = processFile("controls.verificationTest.inputTypeForVerificationTest.invalidMultipleExpressions", configuration)
    output should beResult(Ko[Root](
      InputTypeForVerificationTestControl.invalidType(TypeReference("String"), invalidMultipleExpressionsLocation(14, 7, 8)),
      InputTypeForVerificationTestControl.invalidType(TypeReference("String"), invalidMultipleExpressionsLocation(15, 7, 11)),
      InputTypeForVerificationTestControl.invalidType(TypeReference("String"), invalidMultipleExpressionsLocation(19, 7, 11)),
      InputTypeForVerificationTestControl.invalidType(TypeReference("String"), invalidMultipleExpressionsLocation(20, 7, 8))
    ))
  }

  it should "invalidate a list of invalid type" in {
    val output = processFile("controls.verificationTest.inputTypeForVerificationTest.invalidListType", configuration)
    output should beResult(Ko[Root](
      InputTypeForVerificationTestControl.invalidType(TypeReference("List", Seq(TypeReference("String"))), invalidListTypeLocation(12, 12, 27)),
      InputTypeForVerificationTestControl.invalidType(TypeReference("List", Seq(TypeReference("String"))), invalidListTypeLocation(13, 12, 27))
    ))
  }
}

object InputTypeForVerificationTestControlSpec {
  val configuration = ConfigurationBuilder().withOnlyControls(InputTypeForVerificationTestControl).build()

  val controlDirectory = s"verificationTest/${InputTypeForVerificationTestControl.name}"
  val invalidTypeLocation = LocationPath.control(controlDirectory, "invalidType")
  val invalidMultipleExpressionsLocation = LocationPath.control(controlDirectory, "invalidMultipleExpressions")
  val invalidListTypeLocation = LocationPath.control(controlDirectory, "invalidListType")
}