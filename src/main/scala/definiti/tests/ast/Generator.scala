package definiti.tests.ast

import definiti.common.ast.Location

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