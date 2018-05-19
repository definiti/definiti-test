package definiti.tests.ast

import definiti.common.ast.Location

sealed trait Expression {
  def location: Location
}

case class BooleanExpression(value: Boolean, location: Location) extends Expression

case class NumberExpression(value: BigDecimal, location: Location) extends Expression

case class StringExpression(value: String, location: Location) extends Expression

case class GenerationExpression(name: String, generics: Seq[Type], arguments: Seq[Expression], location: Location) extends Expression

case class StructureExpression(typ: Type, fields: Seq[Field], location: Location) extends Expression

case class Field(name: String, expression: Expression, location: Location)

case class Reference(target: String, location: Location) extends Expression

case class MethodCall(inner: Expression, method: String, generics: Seq[Type], arguments: Seq[Expression], location: Location) extends Expression

case class AttributeCall(inner: Expression, attribute: String, location: Location) extends Expression

case class Condition(condition: Expression, thenCase: Expression, elseCase: Expression, location: Location) extends Expression

case class Binary(operator: BinaryOperator.Value, left: Expression, right: Expression, location: Location) extends Expression

object BinaryOperator extends Enumeration {
  val or, and, lower, lowerOrEqual, upper, upperOrEqual, equal, different, plus, minus, time, divide, modulo = Value

  def from(raw: String): BinaryOperator.Value = raw match {
    case "||" => or
    case "&&" => and
    case "<" => lower
    case "<=" => lowerOrEqual
    case ">" => upper
    case ">=" => upperOrEqual
    case "==" => equal
    case "!=" => different
    case "+" => plus
    case "-" => minus
    case "*" => time
    case "/" => divide
    case "%" => modulo
    case _ => throw new NoSuchElementException(s"No value found for '$raw'")
  }

  def isConditional(operator: BinaryOperator.Value): Boolean = {
    operator == or || operator == and
  }

  def isEquality(operator: BinaryOperator.Value): Boolean = {
    operator == equal || operator == different
  }

  def isInequality(operator: BinaryOperator.Value): Boolean = {
    operator == lower || operator == lowerOrEqual || operator == upper || operator == upperOrEqual
  }

  def isComputation(operator: BinaryOperator.Value): Boolean = {
    operator == plus || operator == minus || operator == time || operator == divide || operator == modulo
  }
}