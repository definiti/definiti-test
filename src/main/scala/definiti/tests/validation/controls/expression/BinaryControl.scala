package definiti.tests.validation.controls.expression

import definiti.common.ast.{Library, Location}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.ast.{Binary, BinaryOperator, Type}
import definiti.tests.validation.ValidationContext
import definiti.tests.validation.helpers.ScopedExpression

object BinaryControl extends Control[ValidationContext] {
  override def description: String = "Check if left and right expressions are valid for binary expressions"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: ValidationContext, library: Library): ControlResult = {
    context.extractExpressions {
      case binary: Binary => binary
    }
      .map(controlBinary)
  }

  private def controlBinary(binary: ScopedExpression[Binary]): ControlResult = {
    if (BinaryOperator.isConditional(binary.operator)) {
      controlBooleanExpressions(binary)
    } else if (BinaryOperator.isEquality(binary.operator)) {
      controlIdenticalExpression(binary)
    } else {
      controlNumberExpressions(binary)
    }
  }

  private def controlBooleanExpressions(binary: ScopedExpression[Binary]): ControlResult = {
    Seq(binary.left, binary.right).map { expression =>
      if (expression.typeOfExpression == Type("Boolean")) {
        ControlResult.OK
      } else {
        ControlResult(invalidType(Type("Boolean"), expression.typeOfExpression, expression.location))
      }
    }
  }

  private def controlIdenticalExpression(binary: ScopedExpression[Binary]): ControlResult = {
    if (binary.left.typeOfExpression == binary.right.typeOfExpression) {
      ControlResult.OK
    } else {
      differentType(binary.left.typeOfExpression, binary.right.typeOfExpression, binary.location)
    }
  }

  private def controlNumberExpressions(binary: ScopedExpression[Binary]): ControlResult = {
    Seq(binary.left, binary.right).map { expression =>
      if (expression.typeOfExpression == Type("Number")) {
        ControlResult.OK
      } else {
        ControlResult(invalidType(Type("Number"), expression.typeOfExpression, expression.location))
      }
    }
  }

  def invalidType(expected: Type, got: Type, location: Location): Alert = {
    alert(s"Invalid type (expected: ${expected.readableString}, got: ${got.readableString})", location)
  }

  def differentType(left: Type, right: Type, location: Location): Alert = {
    alert(s"Different types (left: ${left.readableString}, right: ${right.readableString})", location)
  }
}
