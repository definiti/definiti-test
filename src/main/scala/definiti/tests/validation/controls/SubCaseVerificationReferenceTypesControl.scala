package definiti.tests.validation.controls

import definiti.common.ast._
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.AST
import definiti.tests.AST.{Case, SubCase, TestVerification, TestsContext}
import definiti.tests.validation.helpers.{ExpressionTypes, ScopedType}

object SubCaseVerificationReferenceTypesControl extends Control[TestsContext] {
  override def description: String = "Control that sub case to a verification have the right input types"

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
    if (verification.parameters.length == subCase.arguments.length) {
      verification.parameters.zip(subCase.arguments)
        .map { case (verificationParameter, caseArgument) =>
          controlExpression(caseArgument, ScopedType(verificationParameter.typeReference, verification))
        }
    } else {
      invalidNumberOfParameters(verification.parameters.length, subCase.arguments.length, subCase.location)
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
    alert(s"The number of parameters for verification does not match (expected: ${expectedNumber}, got: ${gotNumber})", location)
  }

  def invalidType(expectedType: AbstractTypeReference, location: Location): Alert = {
    alert(s"The expression does not match the expected type ${expectedType.readableString}", location)
  }
}
