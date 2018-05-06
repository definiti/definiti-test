package definiti.tests.validation.controls

import definiti.core.Alert
import definiti.core.ast._
import definiti.core.validation.{ControlLevel, ControlResult}
import definiti.tests.AST
import definiti.tests.AST.{Case, SubCase, TestVerification, TestsContext}
import definiti.tests.validation.Control
import definiti.tests.validation.helpers.{ExpressionTypes, ScopedType}

object SubCaseVerificationMessageTypesControl extends Control {
  override def description: String = "Control that sub case to a verification have the right message types ('as' part)"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: AST.TestsContext, library: Library): ControlResult = {
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
    ControlResult.squash {
      testCase.subCases.map(controlTestSubCase(_, verification))
    }
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
      ControlResult.squash {
        typedMessage.types.zip(subCase.messageArguments)
          .map { case (messageParameterType, caseArgument) =>
            controlExpression(caseArgument, ScopedType(messageParameterType))
          }
      }
    } else {
      invalidNumberOfParameters(typedMessage.types.length, subCase.messageArguments.length, subCase.location)
    }
  }

  private def controlExpression(expression: AST.Expression, scopedType: ScopedType): ControlResult = {
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
