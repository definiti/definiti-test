package definiti.tests.end2end.controls.verificationTest

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.LocationPath
import definiti.tests.ConfigurationBuilder
import definiti.tests.end2end.EndToEndSpec
import definiti.tests.validation.controls.verificationTest.VerificationReferenceForVerificationTestControl

class VerificationReferenceForVerificationTestControlSpec extends EndToEndSpec {
  import VerificationReferenceForVerificationTestControlSpec._

  "TestsValidation" should "validate a reference to a verification" in {
    val output = processFile("controls.verificationTest.verificationReferenceForVerificationTest.validReference", configuration)
    output shouldBe ok[Root]
  }

  it should "validate a reference to a verification when a package in provided" in {
    val output = processFile("controls.verificationTest.verificationReferenceForVerificationTest.validReferenceInPackage", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a reference to a verification when the verification does not exist" in {
    val output = processFile("controls.verificationTest.verificationReferenceForVerificationTest.invalidReference", configuration)
    output should beResult(Ko[Root](
      VerificationReferenceForVerificationTestControl.unknownReference("Unknown", invalidReferenceLocation(4, 1, 29))
    ))
  }
}

object VerificationReferenceForVerificationTestControlSpec {
  val configuration = ConfigurationBuilder().withOnlyControls(VerificationReferenceForVerificationTestControl).build()

  val controlDirectory = s"verificationTest/${VerificationReferenceForVerificationTestControl.name}"
  val invalidReferenceLocation = LocationPath.control(controlDirectory, "invalidReference")
}