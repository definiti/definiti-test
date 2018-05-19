package definiti.tests.end2end.controls.expression

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.LocationPath
import definiti.tests.ConfigurationBuilder
import definiti.tests.end2end.EndToEndSpec
import definiti.tests.validation.controls.expression.ReferenceControl

class ReferenceControlSpec extends EndToEndSpec {
  import ReferenceControlSpec._

  "TestsValidation" should "validate a call to an attribute" in {
    val output = processFile("controls.expression.reference.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a call to an attribute if it does not exist" in {
    val output = processFile("controls.expression.reference.unknownReference", configuration)
    output should beResult(Ko[Root](
      ReferenceControl.unknownReference("unknown", unknownReferenceLocation(2, 31, 38)),
      ReferenceControl.unknownReference("maxValue", unknownReferenceLocation(4, 70, 78))
    ))
  }
}

object ReferenceControlSpec {
  val configuration = ConfigurationBuilder().withOnlyControls(ReferenceControl).build()

  val controlDirectory = s"expression/${ReferenceControl.name}"
  val unknownReferenceLocation = LocationPath.control(controlDirectory, "unknownReference")
}
