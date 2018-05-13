package definiti.tests.end2end.controls

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.LocationPath
import definiti.tests.ConfigurationBuilder
import definiti.tests.end2end.EndToEndSpec
import definiti.tests.utils.CommonTypes._
import definiti.tests.validation.controls.ValidGenerationControl

class ValidGenerationControlSpec extends EndToEndSpec {
  import ValidGenerationControlSpec._

  "TestsValidation" should "validate valid List constructor" in {
    val output = processFile("controls.validGeneration.nominalList", configuration)
    output shouldBe ok[Root]
  }

  it should "validate valid List constructor for types" in {
    val output = processFile("controls.validGeneration.nominalListForType", configuration)
    output shouldBe ok[Root]
  }

  it should "validate valid Option constructor" in {
    val output = processFile("controls.validGeneration.nominalOption", configuration)
    output shouldBe ok[Root]
  }

  it should "validate valid deep List constructor" in {
    val output = processFile("controls.validGeneration.validDeepTypeForList", configuration)
    output shouldBe ok[Root]
  }

  it should "validate valid deep Option constructor" in {
    val output = processFile("controls.validGeneration.validDeepTypeForOption", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate Option with more than two arguments" in {
    val output = processFile("controls.validGeneration.invalidNumberOfArgumentsForOption", configuration)
    output should beResult(Ko[Root](
      ValidGenerationControl.invalidNumberOfArgument(1, 2, invalidNumberOfArgumentsForOptionLocation(12, 12, 32)),
      ValidGenerationControl.invalidNumberOfArgument(1, 5, invalidNumberOfArgumentsForOptionLocation(13, 12, 44))
    ))
  }

  it should "invalidate an Option with invalid direct type" in {
    val output = processFile("controls.validGeneration.invalidDirectTypeForOption", configuration)
    output should beResult(Ko[Root](
      ValidGenerationControl.unexpectedType(string, number, invalidDirectTypeForOptionLocation(12, 25, 26)),
      ValidGenerationControl.unexpectedType(number, string, invalidDirectTypeForOptionLocation(13, 25, 27))
    ))
  }

  it should "invalidate a List with invalid direct type" in {
    val output = processFile("controls.validGeneration.invalidDirectTypeForList", configuration)
    output should beResult(Ko[Root](
      ValidGenerationControl.unexpectedType(string, number, invalidDirectTypeForListLocation(12, 25, 26)),
      ValidGenerationControl.unexpectedType(number, string, invalidDirectTypeForListLocation(13, 25, 27)),
      ValidGenerationControl.unexpectedType(string, number, invalidDirectTypeForListLocation(14, 29, 30)),
      ValidGenerationControl.unexpectedType(number, string, invalidDirectTypeForListLocation(15, 28, 30)),
      ValidGenerationControl.unexpectedType(number, string, invalidDirectTypeForListLocation(15, 32, 34))
    ))
  }

  it should "invalidate a List with invalid direct type for types" in {
    val output = processFile("controls.validGeneration.invalidDirectTypeForListForType", configuration)
    output should beResult(Ko[Root](
      ValidGenerationControl.unexpectedType(string, number, invalidDirectTypeForListForTypeLocation(10, 27, 28)),
      ValidGenerationControl.unexpectedType(number, string, invalidDirectTypeForListForTypeLocation(13, 27, 29)),
      ValidGenerationControl.unexpectedType(string, number, invalidDirectTypeForListForTypeLocation(16, 31, 32)),
      ValidGenerationControl.unexpectedType(number, string, invalidDirectTypeForListForTypeLocation(19, 30, 32)),
      ValidGenerationControl.unexpectedType(number, string, invalidDirectTypeForListForTypeLocation(19, 34, 36))
    ))
  }

  it should "invalidate an Option with invalid deep type" in {
    val output = processFile("controls.validGeneration.invalidDeepTypeForOption", configuration)
    output should beResult(Ko[Root](
      ValidGenerationControl.unexpectedType(optionOf(string), optionOf(number), invalidDeepTypeForOptionLocation(12, 33, 48)),
      ValidGenerationControl.unexpectedType(string, number, invalidDeepTypeForOptionLocation(13, 46, 47)),
      ValidGenerationControl.unexpectedType(listOf(number), listOf(string), invalidDeepTypeForOptionLocation(14, 31, 47)),
      ValidGenerationControl.unexpectedType(number, string, invalidDeepTypeForOptionLocation(15, 44, 46))
    ))
  }

  it should "invalidate a List with invalid deep type" in {
    val output = processFile("controls.validGeneration.invalidDeepTypeForList", configuration)
    output should beResult(Ko[Root](
      ValidGenerationControl.unexpectedType(listOf(number), listOf(string), invalidDeepTypeForListLocation(12, 31, 47)),
      ValidGenerationControl.unexpectedType(number, string, invalidDeepTypeForListLocation(12, 62, 64)),
      ValidGenerationControl.unexpectedType(listOf(number), optionOf(number), invalidDeepTypeForListLocation(12, 67, 83)),
      ValidGenerationControl.unexpectedType(number, string, invalidDeepTypeForListLocation(12, 80, 82)),

      ValidGenerationControl.unexpectedType(optionOf(string), optionOf(number), invalidDeepTypeForListLocation(13, 33, 48)),
      ValidGenerationControl.unexpectedType(string, number, invalidDeepTypeForListLocation(13, 63, 64)),
      ValidGenerationControl.unexpectedType(optionOf(string), listOf(string), invalidDeepTypeForListLocation(13, 67, 82)),
      ValidGenerationControl.unexpectedType(string, number, invalidDeepTypeForListLocation(13, 80, 81))
    ))
  }

}

object ValidGenerationControlSpec {
  val configuration = ConfigurationBuilder().withOnlyControls(ValidGenerationControl).build()

  val invalidNumberOfArgumentsForOptionLocation = LocationPath.control(ValidGenerationControl, "invalidNumberOfArgumentsForOption")
  val invalidDirectTypeForOptionLocation = LocationPath.control(ValidGenerationControl, "invalidDirectTypeForOption")
  val invalidDirectTypeForListLocation = LocationPath.control(ValidGenerationControl, "invalidDirectTypeForList")
  val invalidDirectTypeForListForTypeLocation = LocationPath.control(ValidGenerationControl, "invalidDirectTypeForListForType")
  val invalidDeepTypeForOptionLocation = LocationPath.control(ValidGenerationControl, "invalidDeepTypeForOption")
  val invalidDeepTypeForListLocation = LocationPath.control(ValidGenerationControl, "invalidDeepTypeForList")
}