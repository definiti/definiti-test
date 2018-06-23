package definiti.tests.end2end.controls.expression

import definiti.common.ast.{Root, TypeReference}
import definiti.common.program.Ko
import definiti.common.tests.LocationPath
import definiti.tests.ConfigurationBuilder
import definiti.tests.end2end.EndToEndSpec
import definiti.tests.utils.CommonTypes.string
import definiti.tests.validation.controls.expression.MethodArgumentsControl

class MethodArgumentsControlSpec extends EndToEndSpec {
  import MethodArgumentsControlSpec._

  "TestsValidation" should "validate a call to a method with right arguments" in {
    val output = processFile("controls.expression.methodArguments.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a call to a method with an invalid number of arguments" in {
    val output = processFile("controls.expression.methodArguments.invalidNumberOfArguments", configuration)
    output should beResult(Ko[Root](
      MethodArgumentsControl.invalidNumberOfArguments(2, 0, invalidNumberOfArgumentsLocation(2, 30, 53)),
      MethodArgumentsControl.invalidNumberOfArguments(2, 1, invalidNumberOfArgumentsLocation(4, 32, 56)),
      MethodArgumentsControl.invalidNumberOfArguments(2, 3, invalidNumberOfArgumentsLocation(6, 32, 62))
    ))
  }

  it should "invalidate a call to an attribute if the inner type if not defined" in {
    val output = processFile("controls.expression.methodArguments.invalidTypeOfArgument", configuration)
    output should beResult(Ko[Root](
      MethodArgumentsControl.invalidTypeOfArgument(TypeReference("Number"), string, invalidTypeOfArgumentLocation(2, 52, 55)),
      MethodArgumentsControl.invalidTypeOfArgument(TypeReference("Number"), string, invalidTypeOfArgumentLocation(2, 57, 60)),
      MethodArgumentsControl.invalidTypeOfArgument(TypeReference("Number"), string, invalidTypeOfArgumentLocation(4, 57, 60)),
      MethodArgumentsControl.invalidTypeOfArgument(TypeReference("Number"), string, invalidTypeOfArgumentLocation(6, 54, 57))
    ))
  }

  it should "accept an argument with the valid type of a generic" in {
    val output = processFile("controls.expression.methodArguments.validParameterWithGenerics", configuration)
    output shouldBe ok[Root]
  }

  it should "refuse an argument with the invalid type of a generic" in {
    val output = processFile("controls.expression.methodArguments.invalidParameterWithGenerics", configuration)
    output shouldBe ko[Root]
  }
}

object MethodArgumentsControlSpec {
  val configuration = ConfigurationBuilder().withOnlyControls(MethodArgumentsControl).build()

  val controlDirectory = s"expression/${MethodArgumentsControl.name}"
  val invalidNumberOfArgumentsLocation = LocationPath.control(controlDirectory, "invalidNumberOfArguments")
  val invalidTypeOfArgumentLocation = LocationPath.control(controlDirectory, "invalidTypeOfArgument")
}

