package definiti.tests.end2end.controls.generator

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.LocationPath
import definiti.tests.ConfigurationBuilder
import definiti.tests.ast.Type
import definiti.tests.end2end.EndToEndSpec
import definiti.tests.utils.CommonTypes._
import definiti.tests.validation.controls.generator.ExpressionTypeOfGeneratorControl

class ExpressionTypeOfGeneratorControlSpec extends EndToEndSpec {
  import ExpressionTypeOfGeneratorControlSpec._

  "TestsValidation" should "validate valid expression type for native type generator" in {
    val output = processFile("controls.generator.expressionTypeOfGenerator.validTypeForNative", configuration)
    output shouldBe ok[Root]
  }

  it should "validate valid expression type for structure generator" in {
    val output = processFile("controls.generator.expressionTypeOfGenerator.validTypeForStructure", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate an expression different from the declaration for native types" in {
    val output = processFile("controls.generator.expressionTypeOfGenerator.invalidTypeForNative", configuration)
    output should beResult(Ko[Root](
      ExpressionTypeOfGeneratorControl.invalidType(number, string, invalidTypeForNativeLocation(2, 33, 44)),
      ExpressionTypeOfGeneratorControl.invalidType(string, number, invalidTypeForNativeLocation(4, 35, 36))
    ))
  }

  it should "invalidate an expression different from the declaration for structure types" in {
    val output = processFile("controls.generator.expressionTypeOfGenerator.invalidTypeForStructure", configuration)
    output should beResult(Ko[Root](
      ExpressionTypeOfGeneratorControl.invalidType(Type("Person"), string, invalidTypeForStructureLocation(6, 33, 44)),
      ExpressionTypeOfGeneratorControl.invalidType(string, Type("Person"), invalidTypeForStructureLocation(8, 35, 10, 4))
    ))
  }
}

object ExpressionTypeOfGeneratorControlSpec {
  val configuration = ConfigurationBuilder().withOnlyControls(ExpressionTypeOfGeneratorControl).build()

  val controlDirectory = s"generator/${ExpressionTypeOfGeneratorControl.name}"
  val invalidTypeForNativeLocation = LocationPath.control(controlDirectory, "invalidTypeForNative")
  val invalidTypeForStructureLocation = LocationPath.control(controlDirectory, "invalidTypeForStructure")
}
