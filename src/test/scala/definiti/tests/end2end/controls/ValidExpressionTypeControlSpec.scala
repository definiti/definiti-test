package definiti.tests.end2end.controls

import definiti.core.Ko
import definiti.core.ast.{Root, TypeReference}
import definiti.tests.AST.Type
import definiti.tests.ConfigurationMock
import definiti.tests.end2end.EndToEndSpec
import definiti.tests.utils.CommonTypes._
import definiti.tests.validation.controls.{InputTypeForVerificationTestControl, ValidExpressionTypeControl}

class ValidExpressionTypeControlSpec extends EndToEndSpec {
  import ValidExpressionTypeControlSpec._

  "TestsValidation" should "validate valid nominal type" in {
    val output = processFile("controls.validExpressionType.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate unknown type" in {
    val output = processFile("controls.validExpressionType.invalidType", configuration)
    output should beResult(Ko[Root](
      InputTypeForVerificationTestControl.invalidType(TypeReference("String"), invalidTypeLocation(12, 12, 21)),
      ValidExpressionTypeControl.invalidType(Type("Unknown"), invalidTypeLocation(12, 12, 21))
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
      InputTypeForVerificationTestControl.invalidType(TypeReference("Option", Seq(TypeReference("A"))), invalidGenericNumberLocation(12, 12, 36)),
      ValidExpressionTypeControl.invalidType(Type("Option", Type("String"), Type("String")), invalidGenericNumberLocation(12, 12, 36))
    ))
  }
}

object ValidExpressionTypeControlSpec {
  import EndToEndSpec._

  val configuration = ConfigurationMock()

  val invalidTypeLocation = LocationPath.control(ValidExpressionTypeControl.name, "invalidType")
  val invalidListLocation = LocationPath.control(ValidExpressionTypeControl.name, "invalidList")
  val invalidGenericNumberLocation = LocationPath.control(ValidExpressionTypeControl.name, "invalidGenericNumber")
}