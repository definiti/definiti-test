package definiti.tests.validation.controls

import definiti.common.control.Control
import definiti.tests.validation.ValidationContext
import definiti.tests.validation.controls.expression.{StructureControl, ValidExpressionTypeControl, ValidGenerationControl}
import definiti.tests.validation.controls.generator.{ExpressionTypeOfGeneratorControl, TypeReferenceForGeneratorControl}
import definiti.tests.validation.controls.typeTest.{InputTypeForTypeTestControl, TypeReferenceForTypeTestControl}
import definiti.tests.validation.controls.verificationTest._

object Controls {
  val all: Seq[Control[ValidationContext]] = Seq(
    ExpressionTypeOfGeneratorControl,
    InputTypeForTypeTestControl,
    InputTypeForVerificationTestControl,
    StructureControl,
    SubCaseVerificationMessageTypesControl,
    SubCaseVerificationReferenceTypesControl,
    TypeReferenceForGeneratorControl,
    TypeReferenceForTypeTestControl,
    ValidGenerationControl,
    ValidExpressionTypeControl,
    VerificationMessageArgumentsOnlyForRefusedCaseControl,
    VerificationReferenceForVerificationTestControl
  )
}
