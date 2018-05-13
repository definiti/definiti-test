package definiti.tests.end2end.controls.expression

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.LocationPath
import definiti.tests.ConfigurationBuilder
import definiti.tests.ast.Type
import definiti.tests.end2end.EndToEndSpec
import definiti.tests.utils.CommonTypes.{any, string}
import definiti.tests.validation.controls.expression.MethodExistenceControl

class MethodExistenceControlSpec extends EndToEndSpec {
  import MethodExistenceControlSpec._

  "TestsValidation" should "validate a call to a method" in {
    val output = processFile("controls.expression.methodExistence.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a call to a method if it does not exist" in {
    val output = processFile("controls.expression.methodExistence.unknownMethod", configuration)
    output should beResult(Ko[Root](
      MethodExistenceControl.unknownMethod(string, "unknown", unknownMethodLocation(11, 30, 51)),
      MethodExistenceControl.unknownMethod(Type("Person"), "unknown", unknownMethodLocation(13, 37, 16, 14)),
      MethodExistenceControl.unknownMethod(Type("People"), "unknown", unknownMethodLocation(18, 35, 21, 14)),
      MethodExistenceControl.unknownMethod(Type("Result"), "unknown", unknownMethodLocation(23, 34, 50))
    ))
  }

  it should "invalidate a call to a method if the inner type if not defined" in {
    val output = processFile("controls.expression.methodExistence.unknownInnerType", configuration)
    output should beResult(Ko[Root](
      MethodExistenceControl.unknownMethod(any, "nonEmpty", unknownInnerTypeLocation(2, 25, 45))
    ))
  }
}

object MethodExistenceControlSpec {
  val configuration = ConfigurationBuilder().withOnlyControls(MethodExistenceControl).build()

  val controlDirectory = s"expression/${MethodExistenceControl.name}"
  val unknownMethodLocation = LocationPath.control(controlDirectory, "unknownMethod")
  val unknownInnerTypeLocation = LocationPath.control(controlDirectory, "unknownInnerType")
}
