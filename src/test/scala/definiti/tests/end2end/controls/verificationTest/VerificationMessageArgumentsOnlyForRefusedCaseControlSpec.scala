package definiti.tests.end2end.controls.verificationTest

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.LocationPath
import definiti.tests.ConfigurationBuilder
import definiti.tests.end2end.EndToEndSpec
import definiti.tests.validation.controls.verificationTest.VerificationMessageArgumentsOnlyForRefusedCaseControl

class VerificationMessageArgumentsOnlyForRefusedCaseControlSpec extends EndToEndSpec {
  import VerificationMessageArgumentsOnlyForRefusedCaseControlSpec._

  "TestsValidation" should "validate a 'refuse' sub case with no message argument" in {
    val output = processFile("controls.verificationTest.verificationMessageArgumentsOnlyForRefusedCase.noMessageArgumentsOnRefuse", configuration)
    output shouldBe ok[Root]
  }

  it should "validate a 'accept' sub case with no message argument" in {
    val output = processFile("controls.verificationTest.verificationMessageArgumentsOnlyForRefusedCase.noMessageArgumentsOnAccept", configuration)
    output shouldBe ok[Root]
  }

  it should "validate a 'refuse' sub case with message argument" in {
    val output = processFile("controls.verificationTest.verificationMessageArgumentsOnlyForRefusedCase.messageArgumentsOnRefuse", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a 'accept' sub case with message argument" in {
    val output = processFile("controls.verificationTest.verificationMessageArgumentsOnlyForRefusedCase.messageArgumentsOnAccept", configuration)
    output should beResult(Ko[Root](
      VerificationMessageArgumentsOnlyForRefusedCaseControl.unacceptedMessageArguments(messageArgumentsOnAcceptLocation(16, 12, 42))
    ))
  }
}

object VerificationMessageArgumentsOnlyForRefusedCaseControlSpec {
  val configuration = ConfigurationBuilder().withOnlyControls(VerificationMessageArgumentsOnlyForRefusedCaseControl).build()

  val controlDirectory = s"verificationTest/${VerificationMessageArgumentsOnlyForRefusedCaseControl.name}"
  val messageArgumentsOnAcceptLocation = LocationPath.control(controlDirectory, "messageArgumentsOnAccept")
}
