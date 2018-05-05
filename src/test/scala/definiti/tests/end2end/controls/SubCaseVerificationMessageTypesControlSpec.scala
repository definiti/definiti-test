package definiti.tests.end2end.controls

import definiti.core.Ko
import definiti.core.ast.{Root, TypeReference}
import definiti.tests.ConfigurationMock
import definiti.tests.end2end.EndToEndSpec
import definiti.tests.validation.controls.SubCaseVerificationMessageTypesControl


class SubCaseVerificationMessageTypesControlSpec extends EndToEndSpec {
  import SubCaseVerificationMessageTypesControlSpec._

  "TestsValidation" should "validate any sub case with no message argument" in {
    val output = processFile("controls.subCaseVerificationMessageTypes.noMessageArguments", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a sub case having message arguments for literal messages" in {
    val output = processFile("controls.subCaseVerificationMessageTypes.invalidMessageArgumentsForLiteral", configuration)
    output should beResult(Ko[Root](
      SubCaseVerificationMessageTypesControl.invalidNumberOfParameters(0, 1, invalidMessageArgumentsForLiteralLocation(19, 12, 40)),
      SubCaseVerificationMessageTypesControl.invalidNumberOfParameters(0, 1, invalidMessageArgumentsForLiteralLocation(23, 12, 30))
    ))
  }

  it should "invalidate a sub case having an invalid number of message arguments for typed messages" in {
    val output = processFile("controls.subCaseVerificationMessageTypes.invalidNumberOfMessageArgumentsForTyped", configuration)
    output should beResult(Ko[Root](
      SubCaseVerificationMessageTypesControl.invalidNumberOfParameters(1, 2, invalidNumberOfMessageArgumentsForTypedLocation(27, 12, 43)),
      SubCaseVerificationMessageTypesControl.invalidNumberOfParameters(1, 2, invalidNumberOfMessageArgumentsForTypedLocation(31, 12, 33))
    ))
  }

  it should "invalidate a sub case having an invalid argument type for typed messages" in {
    val output = processFile("controls.subCaseVerificationMessageTypes.invalidTypeOfMessageArgumentsForTyped", configuration)
    output should beResult(Ko[Root](
      SubCaseVerificationMessageTypesControl.invalidType(TypeReference("Number"), invalidTypeOfMessageArgumentsForTypedLocation(27, 38, 41)),
      SubCaseVerificationMessageTypesControl.invalidType(TypeReference("Number"), invalidTypeOfMessageArgumentsForTypedLocation(31, 28, 32))
    ))
  }
}

object SubCaseVerificationMessageTypesControlSpec {
  import EndToEndSpec._

  val configuration = ConfigurationMock()

  val invalidMessageArgumentsForLiteralLocation = LocationPath.control(SubCaseVerificationMessageTypesControl.name, "invalidMessageArgumentsForLiteral")
  val invalidNumberOfMessageArgumentsForTypedLocation = LocationPath.control(SubCaseVerificationMessageTypesControl.name, "invalidNumberOfMessageArgumentsForTyped")
  val invalidTypeOfMessageArgumentsForTypedLocation = LocationPath.control(SubCaseVerificationMessageTypesControl.name, "invalidTypeOfMessageArgumentsForTyped")
}