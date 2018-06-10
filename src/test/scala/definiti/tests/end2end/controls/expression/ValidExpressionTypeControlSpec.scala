package definiti.tests.end2end.controls.expression

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.LocationPath
import definiti.tests.ConfigurationBuilder
import definiti.tests.ast.Type
import definiti.tests.end2end.EndToEndSpec
import definiti.tests.utils.CommonTypes._
import definiti.tests.validation.controls.expression.ValidExpressionTypeControl

class ValidExpressionTypeControlSpec extends EndToEndSpec {
  import ValidExpressionTypeControlSpec._

  "TestsValidation" should "validate valid nominal type" in {
    val output = processFile("controls.expression.validExpressionType.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "validate valid nominal type for type tests" in {
    val output = processFile("controls.expression.validExpressionType.nominalForType", configuration)
    output shouldBe ok[Root]
  }

  it should "validate valid nominal type for generator" in {
    val output = processFile("controls.expression.validExpressionType.nominalForGenerator", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate unknown type" in {
    val output = processFile("controls.expression.validExpressionType.invalidType", configuration)
    output should beResult(Ko[Root](
      ValidExpressionTypeControl.invalidType(Type("Any"), invalidTypeLocation(12, 12, 21))
    ))
  }

  it should "invalidate unknown type for type tests" in {
    val output = processFile("controls.expression.validExpressionType.invalidTypeForType", configuration)
    output should beResult(Ko[Root](
      ValidExpressionTypeControl.invalidType(Type("Any"), invalidTypeForTypeLocation(9, 12, 21))
    ))
  }

  it should "invalidate unknown type for generators" in {
    val output = processFile("controls.expression.validExpressionType.invalidTypeForGenerator", configuration)
    output should beResult(Ko[Root](
      ValidExpressionTypeControl.invalidType(Type("Any"), invalidTypeForGeneratorLocation(8, 33, 42))
    ))
  }

  it should "validate list of valid type" in {
    val output = processFile("controls.expression.validExpressionType.validList", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate list of unknown type" in {
    val output = processFile("controls.expression.validExpressionType.invalidList", configuration)
    output should beResult(Ko[Root](
      ValidExpressionTypeControl.invalidType(listOf("Unknown"), invalidListLocation(12, 12, 36)),
      ValidExpressionTypeControl.invalidType(listOf(listOf("Unknown")), invalidListLocation(13, 12, 33))
    ))
  }

  it should "validate a valid condition" in {
    val output = processFile("controls.expression.validExpressionType.validCondition", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a condition when if and else have two different types" in {
    val output = processFile("controls.expression.validExpressionType.thenAndElseHaveDifferentTypes", configuration)
    output should beResult(Ko[Root](
      ValidExpressionTypeControl.invalidType(any, thenAndElseHaveDifferentTypesLocation(3, 5, 4, 16))
    ))
  }

  it should "validate generators with expression being const or gen" in {
    val output = processFile("controls.expression.validExpressionType.validGenConstTransformation", configuration)
    output shouldBe ok[Root]
  }
}

object ValidExpressionTypeControlSpec {
  val configuration = ConfigurationBuilder().withOnlyControls(ValidExpressionTypeControl).build()

  val controlDirectory = s"expression/${ValidExpressionTypeControl.name}"
  val invalidTypeLocation = LocationPath.control(controlDirectory, "invalidType")
  val invalidTypeForTypeLocation = LocationPath.control(controlDirectory, "invalidTypeForType")
  val invalidTypeForGeneratorLocation = LocationPath.control(controlDirectory, "invalidTypeForGenerator")
  val invalidListLocation = LocationPath.control(controlDirectory, "invalidList")
  val invalidGenericNumberLocation = LocationPath.control(controlDirectory, "invalidGenericNumber")
  val thenAndElseHaveDifferentTypesLocation = LocationPath.control(controlDirectory, "thenAndElseHaveDifferentTypes")
}