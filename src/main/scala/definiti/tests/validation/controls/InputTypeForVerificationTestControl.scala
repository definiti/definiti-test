package definiti.tests.validation.controls

import definiti.core.Alert
import definiti.core.ast._
import definiti.core.validation.{ControlLevel, ControlResult}
import definiti.tests.AST
import definiti.tests.AST._
import definiti.tests.validation.Control
import definiti.tests.validation.helpers.ExpressionTypes

object InputTypeForVerificationTestControl extends Control {
  override def description: String = "Control if given input is the same as verification input"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: TestsContext, library: Library): ControlResult = {
    ControlResult.squash {
      context.testVerifications.map(controlTestVerification(_, context, library))
    }
  }

  private def controlTestVerification(testVerification: TestVerification, context: TestsContext, library: Library): ControlResult = {
    val verificationName = ExpressionTypes.fullVerificationName(testVerification.verification, context, library)
    library.verificationsMap
      .get(verificationName)
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
      testCase.expressions.map(controlExpression(_, typeReference))
    }
  }

  private def controlExpression(expression: AST.Expression, typeReference: AbstractTypeReference): ControlResult = {
    if (ExpressionTypes.expressionIsTypeOf(expression, typeReference)) {
      ControlResult.OK
    } else {
      invalidType(typeReference, expression.location)
    }
  }

  def invalidType(expectedType: AbstractTypeReference, location: Location): Alert = {
    alert(s"The expression does not match the expected type ${expectedType.readableString}", location)
  }
}
