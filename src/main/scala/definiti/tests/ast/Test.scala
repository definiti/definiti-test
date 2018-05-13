package definiti.tests.ast

import definiti.common.ast.Location

sealed trait Test

case class TestVerification(
  verification: String,
  cases: Seq[Case],
  comment: Option[String],
  location: Location
) extends Test

case class TestType(
  typ: Type,
  cases: Seq[Case],
  comment: Option[String],
  location: Location
) extends Test

case class Case(
  kind: CaseKind.Value,
  subCases: Seq[SubCase],
  comment: Option[String],
  location: Location
)

object CaseKind extends Enumeration {
  val accept, refuse = Value
}

case class SubCase(
  expression: Expression,
  arguments: Seq[Expression],
  messageArguments: Seq[Expression],
  location: Location
)
