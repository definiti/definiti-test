package definiti.tests.validation.controls

import definiti.common.control.Control
import definiti.tests.validation.ValidationContext
import definiti.tests.validation.controls.expression._
import definiti.tests.validation.controls.generator._
import definiti.tests.validation.controls.typeTest._
import definiti.tests.validation.controls.verificationTest._

object Controls {
  val all: Seq[Control[ValidationContext]] = Seq(
    AttributeExistenceControl,
    ExpressionTypeOfGeneratorControl,
    InputTypeForTypeTestControl,
    InputTypeForVerificationTestControl,
    MethodArgumentsControl,
    MethodExistenceControl,
    ReferenceControl,
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
