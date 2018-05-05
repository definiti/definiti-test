package definiti.tests.end2end.controls

import definiti.core.Ko
import definiti.core.ast.{Root, TypeReference}
import definiti.tests.ConfigurationMock
import definiti.tests.end2end.EndToEndSpec
import definiti.tests.validation.controls.SubCaseVerificationReferenceTypesControl

class SubCaseVerificationReferenceTypesControlSpec extends EndToEndSpec {
  import SubCaseVerificationReferenceTypesControlSpec._

  "TestsValidation" should "validate when there is no parameter, nor arguments" in {
    val output = processFile("controls.subCaseVerificationReferenceTypes.validNoParameter", configuration)
    output shouldBe ok[Root]
  }

  it should "validate when there is valid parameters and arguments" in {
    val output = processFile("controls.subCaseVerificationReferenceTypes.validParametersAndArguments", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate when there is one parameter and no argument" in {
    val output = processFile("controls.subCaseVerificationReferenceTypes.oneParameterAndNoArgument", configuration)
    output should beResult(Ko[Root](
      SubCaseVerificationReferenceTypesControl.invalidNumberOfParameters(1, 0, oneParameterAndNoArgumentLocation(13, 7, 19)),
      SubCaseVerificationReferenceTypesControl.invalidNumberOfParameters(1, 0, oneParameterAndNoArgumentLocation(14, 7, 27))
    ))
  }

  it should "invalidate when there is no parameter and one argument" in {
    val output = processFile("controls.subCaseVerificationReferenceTypes.noParameterAndOneArgument", configuration)
    output should beResult(Ko[Root](
      SubCaseVerificationReferenceTypesControl.invalidNumberOfParameters(0, 1, noParameterAndOneArgumentLocation(12, 12, 34))
    ))
  }

  it should "invalidate when there is invalid type of parameters" in {
    val output = processFile("controls.subCaseVerificationReferenceTypes.parametersAndInvalidArguments", configuration)
    output should beResult(Ko[Root](
      SubCaseVerificationReferenceTypesControl.invalidType(TypeReference("Number"), parametersAndInvalidArgumentsLocation(12, 31, 33)),
      SubCaseVerificationReferenceTypesControl.invalidType(TypeReference("Number"), parametersAndInvalidArgumentsLocation(13, 31, 35))
    ))
  }
}

object SubCaseVerificationReferenceTypesControlSpec {
  import EndToEndSpec._

  val configuration = ConfigurationMock()

  val oneParameterAndNoArgumentLocation = LocationPath.control(SubCaseVerificationReferenceTypesControl.name, "oneParameterAndNoArgument")
  val noParameterAndOneArgumentLocation = LocationPath.control(SubCaseVerificationReferenceTypesControl.name, "noParameterAndOneArgument")
  val parametersAndInvalidArgumentsLocation = LocationPath.control(SubCaseVerificationReferenceTypesControl.name, "parametersAndInvalidArguments")
}