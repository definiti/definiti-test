package definiti.tests.end2end.controls.expression

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.LocationPath
import definiti.tests.ConfigurationBuilder
import definiti.tests.end2end.EndToEndSpec
import definiti.tests.utils.CommonTypes._
import definiti.tests.validation.controls.expression.BinaryControl

class BinaryControlSpec extends EndToEndSpec {
  import BinaryControlSpec._

  "TestsValidation" should "validate valid binary expressions" in {
    val output = processFile("controls.expression.binary.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate inequality with invalid left or right types" in {
    val output = processFile("controls.expression.binary.invalidInequality", configuration)
    output should beResult(Ko[Root](
      BinaryControl.invalidType(number, string, invalidInequalityLocation(3, 17, 25)),
      BinaryControl.invalidType(number, string, invalidInequalityLocation(7, 9, 16)),
      BinaryControl.invalidType(number, boolean, invalidInequalityLocation(11, 9, 14)),
      BinaryControl.invalidType(number, string, invalidInequalityLocation(11, 18, 24))
    ))
  }

  it should "invalidate equality with different left and right types" in {
    val output = processFile("controls.expression.binary.invalidEquality", configuration)
    output should beResult(Ko[Root](
      BinaryControl.differentType(string, number, invalidEqualityLocation(3, 5, 20)),
      BinaryControl.differentType(string, number, invalidEqualityLocation(6, 5, 15))
    ))
  }

  it should "invalidate computation with invalid left or right types" in {
    val output = processFile("controls.expression.binary.invalidComputation", configuration)
    output should beResult(Ko[Root](
      BinaryControl.invalidType(number, string, invalidComputationLocation(3, 13, 21)),
      BinaryControl.invalidType(number, string, invalidComputationLocation(6, 5, 12)),
      BinaryControl.invalidType(number, boolean, invalidComputationLocation(9, 5, 10))
    ))
  }

  it should "invalidate conditions with invalid left or right types" in {
    val output = processFile("controls.expression.binary.invalidConditional", configuration)
    output should beResult(Ko[Root](
      BinaryControl.invalidType(boolean, number, invalidConditionalLocation(3, 27, 28)),
      BinaryControl.invalidType(boolean, string, invalidConditionalLocation(7, 9, 25))
    ))
  }
}

object BinaryControlSpec {
  val configuration = ConfigurationBuilder().withOnlyControls(BinaryControl).build()

  val controlDirectory = s"expression/${BinaryControl.name}"
  val invalidInequalityLocation = LocationPath.control(controlDirectory, "invalidInequality")
  val invalidEqualityLocation = LocationPath.control(controlDirectory, "invalidEquality")
  val invalidComputationLocation = LocationPath.control(controlDirectory, "invalidComputation")
  val invalidConditionalLocation = LocationPath.control(controlDirectory, "invalidConditional")
}