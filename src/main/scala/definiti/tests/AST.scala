package definiti.tests

import definiti.core.ast.Location

object AST {
  case class TestsContext(
    tests: Seq[Test]
  )

  sealed trait Test

  case class TestVerification(
    verification: String,
    cases: Seq[Case],
    comment: Option[String],
    location: Location
  ) extends Test

  case class Case(
    kind: CaseKind.Value,
    expressions: Seq[Expression],
    comment: Option[String],
    location: Location
  )

  object CaseKind extends Enumeration {
    val accept, refuse = Value
  }

  sealed trait Expression {
    def location: Location
  }

  case class BooleanExpression(value: Boolean, location: Location) extends Expression

  case class NumberExpression(value: BigDecimal, location: Location) extends Expression

  case class StringExpression(value: String, location: Location) extends Expression
}
