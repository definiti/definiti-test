package definiti.tests.validation.helpers

import definiti.core.ast.{AbstractTypeReference, Library, TypeReference}
import definiti.tests.AST._

object ExpressionTypes {
  private val boolean = TypeReference("Boolean")
  private val number = TypeReference("Number")
  private val string = TypeReference("String")

  def expressionIsTypeOf(expression: Expression, typeReference: TypeReference): Boolean = {
    expression match {
      case _: BooleanExpression => typeReference == boolean
      case _: NumberExpression => typeReference == number
      case _: StringExpression => typeReference == string
    }
  }

  def expressionIsTypeOf(expression: Expression, abstractTypeReference: AbstractTypeReference): Boolean = {
    abstractTypeReference match {
      case typeReference: TypeReference => expressionIsTypeOf(expression, typeReference)
      case _ => false
    }
  }

  def fullVerificationName(verificationName: String, context: TestsContext, library: Library): String = {
    PackageFinder.packageOfContext(context, library) match {
      case "" => verificationName
      case namespace => namespace + "." + verificationName
    }
  }
}
