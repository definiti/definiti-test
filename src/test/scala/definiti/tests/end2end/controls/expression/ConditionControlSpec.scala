package definiti.tests.end2end.controls.expression

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.LocationPath
import definiti.tests.ConfigurationBuilder
import definiti.tests.end2end.EndToEndSpec
import definiti.tests.utils.CommonTypes._
import definiti.tests.validation.controls.expression.ConditionControl

class ConditionControlSpec extends EndToEndSpec {
  import ConditionControlSpec._

  "TestsValidation" should "validate a control with boolean condition" in {
    val output = processFile("controls.expression.condition.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a control with a non-boolean condition" in {
    val output = processFile("controls.expression.condition.invalidCondition", configuration)
    output should beResult(Ko[Root](
      ConditionControl.invalidBoolean(number, invalidConditionLocation(3, 9, 24))
    ))
  }
}

object ConditionControlSpec {
  val configuration = ConfigurationBuilder().withOnlyControls(ConditionControl).build()

  val controlDirectory = s"expression/${ConditionControl.name}"
  val invalidConditionLocation = LocationPath.control(controlDirectory, "invalidCondition")
}