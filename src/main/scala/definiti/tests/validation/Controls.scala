package definiti.tests.validation

import definiti.common.control.Control
import definiti.tests.validation.controls._

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
