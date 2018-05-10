package definiti.tests.validation

import definiti.common.control.Control
import definiti.tests.AST.TestsContext
import definiti.tests.validation.controls._

object Controls {
  val all: Seq[Control[TestsContext]] = Seq(
    InputTypeForTypeTestControl,
    InputTypeForVerificationTestControl,
    StructureControl,
    SubCaseVerificationMessageTypesControl,
    SubCaseVerificationReferenceTypesControl,
    TypeReferenceForTypeTestControl,
    ValidConstructorControl,
    ValidExpressionTypeControl,
    VerificationMessageArgumentsOnlyForRefusedCaseControl,
    VerificationReferenceForVerificationTestControl
  )
}
