package definiti.tests.validation.controls

import definiti.common.ast.{AbstractTypeReference, Library, Location, Verification}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.AST._
import definiti.tests.validation.ValidationContext
import definiti.tests.validation.helpers.{ExpressionTypes, ScopedType}

object InputTypeForVerificationTestControl extends Control[ValidationContext] {
  override def description: String = "Control if given input is the same as verification input"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: ValidationContext, library: Library): ControlResult = {
    context.testVerifications.map(controlTestVerification(_, context))
  }

  private def controlTestVerification(testVerification: TestVerification, context: ValidationContext): ControlResult = {
    context.getVerification(testVerification.verification)
      .map { verification =>
        ControlResult.squash {
          testVerification.cases.map(controlTestCase(_, verification, context))
        }
      }
      .getOrElse(ignored)
  }

  private def controlTestCase(testCase: Case, verification: Verification, context: ValidationContext): ControlResult = {
    val typeReference = verification.function.parameters.head.typeReference
    testCase.subCases.map(subCase => controlExpression(subCase.expression, ScopedType(typeReference, verification), context))
  }

  private def controlExpression(expression: Expression, scopedType: ScopedType, context: ValidationContext): ControlResult = {
    if (scopedType.isSameAs(ExpressionTypes.getTypeOfExpression(expression, context))) {
      ControlResult.OK
    } else {
      invalidType(scopedType.typeReference, expression.location)
    }
  }

  def invalidType(expectedType: AbstractTypeReference, location: Location): Alert = {
    alert(s"The expression does not match the expected type ${expectedType.readableString}", location)
  }
}
