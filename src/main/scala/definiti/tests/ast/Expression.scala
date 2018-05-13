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