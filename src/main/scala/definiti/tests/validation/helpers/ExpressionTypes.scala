package definiti.tests.validation.helpers

import definiti.core.ast.Library
import definiti.tests.AST._

object ExpressionTypes {
  private val boolean = Type("Boolean", Seq.empty)
  private val number = Type("Number", Seq.empty)
  private val string = Type("String", Seq.empty)

  def getTypeOfExpression(expression: Expression): Type = {
    expression match {
      case _: BooleanExpression => boolean
      case _: NumberExpression => number
      case _: StringExpression => string
      case constructor: ConstructorExpression => constructor.typ
    }
  }

  def fullVerificationName(verificationName: String, context: TestsContext, library: Library): String = {
    PackageFinder.packageOfContext(context, library) match {
      case "" => verificationName
      case namespace => namespace + "." + verificationName
    }
  }
}
