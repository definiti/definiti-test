package definiti.tests.validation

import definiti.common.control.Control
import definiti.tests.AST.TestsContext
import definiti.tests.validation.controls.{InputTypeForVerificationTestControl, StructureControl, SubCaseVerificationMessageTypesControl, SubCaseVerificationReferenceTypesControl, ValidConstructorControl, ValidExpressionTypeControl, VerificationMessageArgumentsOnlyForRefusedCaseControl, VerificationReferenceForVerificationTestControl}

object Controls {
  val all: Seq[Control[TestsContext]] = Seq(
    InputTypeForVerificationTestControl,
    StructureControl,
    SubCaseVerificationMessageTypesControl,
    SubCaseVerificationReferenceTypesControl,
    ValidConstructorControl,
    ValidExpressionTypeControl,
    VerificationMessageArgumentsOnlyForRefusedCaseControl,
    VerificationReferenceForVerificationTestControl
  )
}
