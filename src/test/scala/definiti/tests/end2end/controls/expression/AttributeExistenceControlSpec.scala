package definiti.tests.end2end.controls.expression

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.LocationPath
import definiti.tests.ConfigurationBuilder
import definiti.tests.ast.Type
import definiti.tests.end2end.EndToEndSpec
import definiti.tests.validation.controls.expression.AttributeExistenceControl
import definiti.tests.utils.CommonTypes._

class AttributeExistenceControlSpec extends EndToEndSpec {
  import AttributeExistenceControlSpec._

  "TestsValidation" should "validate a call to an attribute" in {
    val output = processFile("controls.expression.attributeExistence.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a call to an attribute if it does not exist" in {
    val output = processFile("controls.expression.attributeExistence.unknownAttribute", configuration)
    output should beResult(Ko[Root](
      AttributeExistenceControl.unknownAttribute(string, "unknown", unknownAttributeLocation(11, 30, 49)),
      AttributeExistenceControl.unknownAttribute(Type("Person"), "unknown", unknownAttributeLocation(13, 37, 16, 12)),
      AttributeExistenceControl.unknownAttribute(Type("People"), "unknown", unknownAttributeLocation(18, 35, 21, 12)),
      AttributeExistenceControl.unknownAttribute(Type("Result"), "unknown", unknownAttributeLocation(23, 34, 48))
    ))
  }

  it should "invalidate a call to an attribute if the inner type if not defined" in {
    val output = processFile("controls.expression.attributeExistence.unknownInnerType", configuration)
    output should beResult(Ko[Root](
      AttributeExistenceControl.unknownAttribute(any, "length", unknownInnerTypeLocation(2, 25, 41))
    ))
  }
}

object AttributeExistenceControlSpec {
  val configuration = ConfigurationBuilder().withOnlyControls(AttributeExistenceControl).build()

  val controlDirectory = s"expression/${AttributeExistenceControl.name}"
  val unknownAttributeLocation = LocationPath.control(controlDirectory, "unknownAttribute")
  val unknownInnerTypeLocation = LocationPath.control(controlDirectory, "unknownInnerType")
}
