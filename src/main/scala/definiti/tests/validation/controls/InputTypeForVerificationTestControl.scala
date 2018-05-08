package definiti.tests.validation.controls

import definiti.common.ast.{AbstractTypeReference, Library, Location, Verification}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.AST._
import definiti.tests.validation.helpers.{ExpressionTypes, ScopedType}

object InputTypeForVerificationTestControl extends Control[TestsContext] {
  override def description: String = "Control if given input is the same as verification input"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: TestsContext, library: Library): ControlResult = {
    context.testVerifications.map(controlTestVerification(_, context, library))
  }

  private def controlTestVerification(testVerification: TestVerification, context: TestsContext, library: Library): ControlResult = {
    library.verificationsMap
      .get(testVerification.verification)
      .map { verification =>
        ControlResult.squash {
          testVerification.cases.map(controlTestCase(_, verification))
        }
      }
      .getOrElse(ignored)
  }

  private def controlTestCase(testCase: Case, verification: Verification): ControlResult = {
    val typeReference = verification.function.parameters.head.typeReference
    ControlResult.squash {
      testCase.subCases.map(subCase => controlExpression(subCase.expression, ScopedType(typeReference, verification)))
    }
  }

  private def controlExpression(expression: Expression, scopedType: ScopedType): ControlResult = {
    if (scopedType.isSameAs(ExpressionTypes.getTypeOfExpression(expression))) {
      ControlResult.OK
    } else {
      invalidType(scopedType.typeReference, expression.location)
    }
  }

  def invalidType(expectedType: AbstractTypeReference, location: Location): Alert = {
    alert(s"The expression does not match the expected type ${expectedType.readableString}", location)
  }
}
