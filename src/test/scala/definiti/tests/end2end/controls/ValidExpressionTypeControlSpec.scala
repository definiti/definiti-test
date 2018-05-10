package definiti.tests.end2end.controls

import definiti.common.ast.{Root, TypeReference}
import definiti.common.program.Ko
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.tests.AST.Type
import definiti.tests.ConfigurationBuilder
import definiti.tests.end2end.EndToEndSpec
import definiti.tests.utils.CommonTypes._
import definiti.tests.validation.controls.{InputTypeForVerificationTestControl, ValidExpressionTypeControl}

class ValidExpressionTypeControlSpec extends EndToEndSpec {
  import ValidExpressionTypeControlSpec._

  "TestsValidation" should "validate valid nominal type" in {
    val output = processFile("controls.validExpressionType.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "validate valid nominal type for type tests" in {
    val output = processFile("controls.validExpressionType.nominalForType", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate unknown type" in {
    val output = processFile("controls.validExpressionType.invalidType", configuration)
    output should beResult(Ko[Root](
      ValidExpressionTypeControl.invalidType(Type("Unknown"), invalidTypeLocation(12, 12, 21))
    ))
  }

  it should "invalidate unknown type for type tests" in {
    val output = processFile("controls.validExpressionType.invalidTypeForType", configuration)
    output should beResult(Ko[Root](
      ValidExpressionTypeControl.invalidType(Type("Unknown"), invalidTypeForTypeLocation(9, 12, 21))
    ))
  }

  it should "validate list of valid type" in {
    val output = processFile("controls.validExpressionType.validList", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate list of unknown type" in {
    val output = processFile("controls.validExpressionType.invalidList", configuration)
    output should beResult(Ko[Root](
      ValidExpressionTypeControl.invalidType(listOf("Unknown"), invalidListLocation(12, 12, 36)),
      ValidExpressionTypeControl.invalidType(listOf(listOf("Unknown")), invalidListLocation(13, 12, 33))
    ))
  }

  it should "invalidate type with invalid number of generics" in {
    val output = processFile("controls.validExpressionType.invalidGenericNumber", configuration)
    output should beResult(Ko[Root](
      ValidExpressionTypeControl.invalidType(Type("Option", Type("String"), Type("String")), invalidGenericNumberLocation(12, 12, 36))
    ))
  }
}

object ValidExpressionTypeControlSpec {
  val configuration = ConfigurationBuilder().withOnlyControls(ValidExpressionTypeControl).build()

  val invalidTypeLocation = LocationPath.control(ValidExpressionTypeControl, "invalidType")
  val invalidTypeForTypeLocation = LocationPath.control(ValidExpressionTypeControl, "invalidTypeForType")
  val invalidListLocation = LocationPath.control(ValidExpressionTypeControl, "invalidList")
  val invalidGenericNumberLocation = LocationPath.control(ValidExpressionTypeControl, "invalidGenericNumber")
}