package definiti.tests.end2end.controls

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.tests.AST.Type
import definiti.tests.ConfigurationBuilder
import definiti.tests.end2end.EndToEndSpec
import definiti.tests.utils.CommonTypes._
import definiti.tests.validation.controls.ValidConstructorControl

class ValidConstructorControlSpec extends EndToEndSpec {
  import ValidConstructorControlSpec._

  "TestsValidation" should "validate valid List constructor" in {
    val output = processFile("controls.validConstructor.nominalList", configuration)
    output shouldBe ok[Root]
  }

  it should "validate valid List constructor for types" in {
    val output = processFile("controls.validConstructor.nominalListForType", configuration)
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

  it should "invalidate a List with invalid direct type for types" in {
    val output = processFile("controls.validConstructor.invalidDirectTypeForListForType", configuration)
    output should beResult(Ko[Root](
      ValidConstructorControl.unexpectedType(string, number, invalidDirectTypeForListForTypeLocation(10, 27, 28)),
      ValidConstructorControl.unexpectedType(number, string, invalidDirectTypeForListForTypeLocation(13, 27, 29)),
      ValidConstructorControl.unexpectedType(string, number, invalidDirectTypeForListForTypeLocation(16, 31, 32)),
      ValidConstructorControl.unexpectedType(number, string, invalidDirectTypeForListForTypeLocation(19, 30, 32)),
      ValidConstructorControl.unexpectedType(number, string, invalidDirectTypeForListForTypeLocation(19, 34, 36))
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
      ValidConstructorControl.unexpectedType(number, string, invalidDeepTypeForListLocation(12, 82, 84)),

      ValidConstructorControl.unexpectedType(optionOf(string), optionOf(number), invalidDeepTypeForListLocation(13, 33, 50)),
      ValidConstructorControl.unexpectedType(string, number, invalidDeepTypeForListLocation(13, 67, 68)),
      ValidConstructorControl.unexpectedType(optionOf(string), listOf(string), invalidDeepTypeForListLocation(13, 71, 86)),
      ValidConstructorControl.unexpectedType(string, number, invalidDeepTypeForListLocation(13, 84, 85))
    ))
  }

}

object ValidConstructorControlSpec {
  val configuration = ConfigurationBuilder().withOnlyControls(ValidConstructorControl).build()

  val invalidNumberOfArgumentsForOptionLocation = LocationPath.control(ValidConstructorControl, "invalidNumberOfArgumentsForOption")
  val invalidDirectTypeForOptionLocation = LocationPath.control(ValidConstructorControl, "invalidDirectTypeForOption")
  val invalidDirectTypeForListLocation = LocationPath.control(ValidConstructorControl, "invalidDirectTypeForList")
  val invalidDirectTypeForListForTypeLocation = LocationPath.control(ValidConstructorControl, "invalidDirectTypeForListForType")
  val invalidDeepTypeForOptionLocation = LocationPath.control(ValidConstructorControl, "invalidDeepTypeForOption")
  val invalidDeepTypeForListLocation = LocationPath.control(ValidConstructorControl, "invalidDeepTypeForList")
}