package definiti.tests.end2end.controls

import definiti.core.Ko
import definiti.core.ast.{Root, TypeReference}
import definiti.tests.ConfigurationMock
import definiti.tests.end2end.EndToEndSpec
import definiti.tests.validation.controls.InputTypeForVerificationTestControl

class InputTypeForVerificationTestControlSpec extends EndToEndSpec {
  import InputTypeForVerificationTestControlSpec._

  "TestsValidation" should "validate valid string parameter" in {
    val output = processFile("controls.inputTypeForVerificationTest.validString", configuration)
    output shouldBe ok[Root]
  }

  it should "validate valid number parameter" in {
    val output = processFile("controls.inputTypeForVerificationTest.validNumber", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a reference to a verification when the verification does not exist" in {
    val output = processFile("controls.inputTypeForVerificationTest.invalidType", configuration)
    output should beResult(Ko[Root](
      InputTypeForVerificationTestControl.invalidType(TypeReference("String"), invalidTypeLocation(12, 12, 13)),
      InputTypeForVerificationTestControl.invalidType(TypeReference("String"), invalidTypeLocation(13, 12, 17))
    ))
  }
}

object InputTypeForVerificationTestControlSpec {
  import EndToEndSpec._

  val configuration = ConfigurationMock()

  val invalidTypeLocation = LocationPath.control(InputTypeForVerificationTestControl.name, "invalidType")
}