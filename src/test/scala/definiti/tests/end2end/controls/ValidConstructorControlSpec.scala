package definiti.tests.end2end.controls

import definiti.core.Ko
import definiti.core.ast.Root
import definiti.tests.ConfigurationMock
import definiti.tests.end2end.EndToEndSpec
import definiti.tests.utils.CommonTypes._
import definiti.tests.validation.controls.ValidConstructorControl

class ValidConstructorControlSpec extends EndToEndSpec {
  import ValidConstructorControlSpec._

  "TestsValidation" should "validate valid List constructor" in {
    val output = processFile("controls.validConstructor.nominalList", configuration)
    output shouldBe ok[Root]
  }

  it should "validate valid Option constructor" in {
    val output = processFile("controls.validConstructor.nominalOption", configuration)
    output shouldBe ok[Root]
  }

  it should "validate valid deep List constructor" in {
    val output = processFile("controls.validConstructor.validDeepTypeForList", configuration)
    output shouldBe ok[Root]
  }

  it should "validate valid deep Option constructor" in {
    val output = processFile("controls.validConstructor.validDeepTypeForOption", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate Option with more than two arguments" in {
    val output = processFile("controls.validConstructor.invalidNumberOfArgumentsForOption", configuration)
    output should beResult(Ko[Root](
      ValidConstructorControl.invalidNumberOfArgument(Seq(0, 1), 2, invalidNumberOfArgumentsForOptionLocation(12, 12, 34)),
      ValidConstructorControl.invalidNumberOfArgument(Seq(0, 1), 5, invalidNumberOfArgumentsForOptionLocation(13, 12, 46))
    ))
  }

  it should "invalidate an Option with invalid direct type" in {
    val output = processFile("controls.validConstructor.invalidDirectTypeForOption", configuration)
    output should beResult(Ko[Root](
      ValidConstructorControl.unexpectedType(string, number, invalidDirectTypeForOptionLocation(12, 27, 28)),
      ValidConstructorControl.unexpectedType(number, string, invalidDirectTypeForOptionLocation(13, 27, 29))
    ))
  }

  it should "invalidate a List with invalid direct type" in {
    val output = processFile("controls.validConstructor.invalidDirectTypeForList", configuration)
    output should beResult(Ko[Root](
      ValidConstructorControl.unexpectedType(string, number, invalidDirectTypeForListLocation(12, 25, 26)),
      ValidConstructorControl.unexpectedType(number, string, invalidDirectTypeForListLocation(13, 25, 27)),
      ValidConstructorControl.unexpectedType(string, number, invalidDirectTypeForListLocation(14, 29, 30)),
      ValidConstructorControl.unexpectedType(number, string, invalidDirectTypeForListLocation(15, 28, 30)),
      ValidConstructorControl.unexpectedType(number, string, invalidDirectTypeForListLocation(15, 32, 34))
    ))
  }

  it should "invalidate an Option with invalid deep type" in {
    val output = processFile("controls.validConstructor.invalidDeepTypeForOption", configuration)
    output should beResult(Ko[Root](
      ValidConstructorControl.unexpectedType(optionOf(string), optionOf(number), invalidDeepTypeForOptionLocation(12, 35, 52)),
      ValidConstructorControl.unexpectedType(string, number, invalidDeepTypeForOptionLocation(13, 50, 51)),
      ValidConstructorControl.unexpectedType(listOf(number), listOf(string), invalidDeepTypeForOptionLocation(14, 33, 49)),
      ValidConstructorControl.unexpectedType(number, string, invalidDeepTypeForOptionLocation(15, 46, 48))
    ))
  }

  it should "invalidate a List with invalid deep type" in {
    val output = processFile("controls.validConstructor.invalidDeepTypeForList", configuration)
    output should beResult(Ko[Root](
      ValidConstructorControl.unexpectedType(listOf(number), listOf(string), invalidDeepTypeForListLocation(12, 31, 47)),
      ValidConstructorControl.unexpectedType(number, string, invalidDeepTypeForListLocation(12, 62, 64)),
      ValidConstructorControl.unexpectedType(listOf(number), optionOf(number), invalidDeepTypeForListLocation(12, 67, 85)),

      ValidConstructorControl.unexpectedType(optionOf(string), optionOf(number), invalidDeepTypeForListLocation(13, 33, 50)),
      ValidConstructorControl.unexpectedType(string, number, invalidDeepTypeForListLocation(13, 67, 68)),
      ValidConstructorControl.unexpectedType(optionOf(string), listOf(string), invalidDeepTypeForListLocation(13, 71, 86))
    ))
  }

}

object ValidConstructorControlSpec {
  import EndToEndSpec._

  val configuration = ConfigurationMock()

  val invalidNumberOfArgumentsForOptionLocation = LocationPath.control(ValidConstructorControl.name, "invalidNumberOfArgumentsForOption")
  val invalidDirectTypeForOptionLocation = LocationPath.control(ValidConstructorControl.name, "invalidDirectTypeForOption")
  val invalidDirectTypeForListLocation = LocationPath.control(ValidConstructorControl.name, "invalidDirectTypeForList")
  val invalidDeepTypeForOptionLocation = LocationPath.control(ValidConstructorControl.name, "invalidDeepTypeForOption")
  val invalidDeepTypeForListLocation = LocationPath.control(ValidConstructorControl.name, "invalidDeepTypeForList")
}