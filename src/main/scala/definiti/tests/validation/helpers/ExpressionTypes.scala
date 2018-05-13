package definiti.tests.validation.helpers

import definiti.tests.ast._
import definiti.tests.validation.ValidationContext

object ExpressionTypes {
  private val boolean = Type("Boolean", Seq.empty)
  private val number = Type("Number", Seq.empty)
  private val string = Type("String", Seq.empty)
  private val any = Type("Any", Seq.empty)

  def getTypeOfExpression(expression: Expression, validationContext: ValidationContext): Type = {
    expression match {
      case _: BooleanExpression => boolean
      case _: NumberExpression => number
      case _: StringExpression => string
      case generation: GenerationExpression =>
        validationContext.generators.find(_.fullName == generation.name)
          .map { generator =>
            Types.replaceGenerics(generator.typ, generator.generics.zip(generation.generics).toMap)
          }
          .getOrElse(any)
      case structure: StructureExpression => structure.typ
    }
  }
}
