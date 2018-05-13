package definiti.tests

import definiti.common.ast.Location

object AST {
  case class TestsContext(
    tests: Seq[Test],
    generators: Seq[Generator]
  )

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

  sealed trait Expression {
    def location: Location
  }

  case class BooleanExpression(value: Boolean, location: Location) extends Expression

  case class NumberExpression(value: BigDecimal, location: Location) extends Expression

  case class StringExpression(value: String, location: Location) extends Expression

  case class GenerationExpression(name: String, generics: Seq[Type], arguments: Seq[Expression], location: Location) extends Expression

  case class Type(name: String, generics: Seq[Type]) {
    def readableString: String = {
      if (generics.nonEmpty) {
        s"${name}[${generics.map(_.readableString).mkString(", ")}]"
      } else {
        name
      }
    }
  }

  object Type {
    def apply(name: String, generics: Type*)(implicit dummyImplicit: DummyImplicit): Type = new Type(name, generics)
  }

  case class StructureExpression(typ: Type, fields: Seq[Field], location: Location) extends Expression

  case class Field(name: String, expression: Expression, location: Location)

  case class Generator(
    name: String,
    fullName: String,
    generics: Seq[String],
    typ: Type,
    parameters: Seq[Parameter],
    expression: Expression,
    location: Location
  )

  case class Parameter(
    name: String,
    typ: Type,
    isRest: Boolean,
    location: Location
  )
}
