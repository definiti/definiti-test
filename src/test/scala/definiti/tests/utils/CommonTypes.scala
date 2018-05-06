package definiti.tests.utils

import definiti.tests.AST.Type

object CommonTypes {
  val boolean = Type("Boolean")
  val number = Type("Number")
  val string = Type("String")

  def listOf(typ: String) = Type("List", Type(typ))

  def listOf(typ: Type) = Type("List", typ)

  def optionOf(typ: String) = Type("Option", Type(typ))

  def optionOf(typ: Type) = Type("Option", typ)
}
