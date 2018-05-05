package definiti.tests.validation

import definiti.core.ast.Library
import definiti.core.validation.ControlResult
import definiti.tests.AST.TestsContext
import definiti.tests.validation.controls._

class TestsValidation(library: Library) {
  private val controls: Seq[Control] = Seq(
    InputTypeForVerificationTestControl,
    SubCaseVerificationMessageTypesControl,
    SubCaseVerificationReferenceTypesControl,
    VerificationMessageArgumentsOnlyForRefusedCaseControl,
    VerificationReferenceForVerificationTestControl
  )

  def validate(context: TestsContext): ControlResult = {
    ControlResult.squash {
      controls.map(_.control(context, library))
    }
  }
}
