package definiti.tests.validation.controls

import definiti.core.Alert
import definiti.core.ast.{AbstractTypeReference, Library, Location, Verification}
import definiti.core.validation.{ControlLevel, ControlResult}
import definiti.tests.AST
import definiti.tests.AST.{Case, SubCase, TestVerification, TestsContext}
import definiti.tests.validation.Control
import definiti.tests.validation.helpers.ExpressionTypes

object SubCaseVerificationReferenceTypesControl extends Control {
  override def description: String = "Control that sub case to a verification have the right input types"

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
    if (verification.parameters.length == subCase.arguments.length) {
      ControlResult.squash {
        verification.parameters.zip(subCase.arguments)
          .map { case (verificationParameter, caseArgument) =>
            controlExpression(caseArgument, verificationParameter.typeReference)
          }
      }
    } else {
      invalidNumberOfParameters(verification.parameters.length, subCase.arguments.length, subCase.location)
    }
  }

  private def controlExpression(expression: AST.Expression, typeReference: AbstractTypeReference): ControlResult = {
    if (ExpressionTypes.expressionIsTypeOf(expression, typeReference)) {
      ControlResult.OK
    } else {
      invalidType(typeReference, expression.location)
    }
  }

  def invalidNumberOfParameters(expectedNumber: Int, gotNumber: Int, location: Location): Alert = {
    alert(s"The number of parameters for verification does not match (expected: ${expectedNumber}, got: ${gotNumber})", location)
  }

  def invalidType(expectedType: AbstractTypeReference, location: Location): Alert = {
    alert(s"The expression does not match the expected type ${expectedType.readableString}", location)
  }
}
