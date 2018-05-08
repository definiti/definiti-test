package definiti.tests.validation.controls

import definiti.common.ast.{Expression => _, _}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.AST._
import definiti.tests.validation.helpers.{ExpressionTypes, ScopedType}

object SubCaseVerificationMessageTypesControl extends Control[TestsContext] {
  override def description: String = "Control that sub case to a verification have the right message types ('as' part)"

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
    testCase.subCases.map(controlTestSubCase(_, verification))
  }

  private def controlTestSubCase(subCase: SubCase, verification: Verification): ControlResult = {
    if (subCase.messageArguments.isEmpty) {
      // The emptiness means the developer does not want to check returned values of message
      ControlResult.OK
    } else {
      verification.message match {
        case _: LiteralMessage =>
          invalidNumberOfParameters(0, subCase.messageArguments.length, subCase.location)
        case typedMessage: TypedMessage =>
          controlWithTypedMessage(subCase, typedMessage)
      }
    }
  }

  private def controlWithTypedMessage(subCase: SubCase, typedMessage: TypedMessage): ControlResult = {
    if (typedMessage.types.length == subCase.messageArguments.length) {
      typedMessage.types.zip(subCase.messageArguments)
        .map { case (messageParameterType, caseArgument) =>
          controlExpression(caseArgument, ScopedType(messageParameterType))
        }
    } else {
      invalidNumberOfParameters(typedMessage.types.length, subCase.messageArguments.length, subCase.location)
    }
  }

  private def controlExpression(expression: Expression, scopedType: ScopedType): ControlResult = {
    if (scopedType.isSameAs(ExpressionTypes.getTypeOfExpression(expression))) {
      ControlResult.OK
    } else {
      invalidType(scopedType.typeReference, expression.location)
    }
  }

  def invalidNumberOfParameters(expectedNumber: Int, gotNumber: Int, location: Location): Alert = {
    alert(s"The number of parameters for message verification does not match (expected: ${expectedNumber}, got: ${gotNumber})", location)
  }

  def invalidType(expectedType: AbstractTypeReference, location: Location): Alert = {
    alert(s"The expression does not match the expected type ${expectedType.readableString}", location)
  }
}
