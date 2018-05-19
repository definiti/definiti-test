package definiti.tests.validation.helpers

import definiti.common.ast.{DefinedType, Enum, Location, NativeClassDefinition}
import definiti.tests.ast._
import definiti.tests.validation.ValidationContext

case class ScopedExpression[E <: Expression](expression: E, references: Map[String, Type], context: ValidationContext) {
  private val boolean = Type("Boolean", Seq.empty)
  private val number = Type("Number", Seq.empty)
  private val string = Type("String", Seq.empty)
  private val any = Type("Any", Seq.empty)

  def typeOfExpression: Type = process(expression)

  def location: Location = expression.location

  private def process(expression: Expression): Type = {
    expression match {
      case _: BooleanExpression => boolean
      case _: NumberExpression => number
      case _: StringExpression => string
      case generation: GenerationExpression =>
        context.generators.find(_.fullName == generation.name)
          .map { generator =>
            Types.replaceGenerics(generator.typ, generator.generics.zip(generation.generics).toMap)
          }
          .getOrElse(any)
      case structure: StructureExpression => structure.typ
      case methodCall: MethodCall =>
        val innerExpressionType = process(methodCall.inner)
        context.getFinalClassDefinition(innerExpressionType.name) match {
          case Some(native: NativeClassDefinition) =>
            (for {
              method <- native.methods.find(_.name == methodCall.method)
              returnType = Types.typeReferenceToType(method.returnType)
              finalType = Types.replaceGenerics(returnType, method.genericTypes.zip(methodCall.generics).toMap)
            } yield {
              finalType
            })
              .getOrElse(any)
          case _ => any
        }
      case attributeCall: AttributeCall =>
        val innerExpressionType = process(attributeCall.inner)
        context.getFinalClassDefinition(innerExpressionType.name) match {
          case Some(native: NativeClassDefinition) =>
            native.attributes
              .find(_.name == attributeCall.attribute)
              .map(_.typeDeclaration)
              .map(Types.typeDeclarationToType)
              .getOrElse(any)
          case Some(definedType: DefinedType) =>
            definedType.attributes
              .find(_.name == attributeCall.attribute)
              .map(_.typeDeclaration)
              .map(Types.typeDeclarationToType)
              .getOrElse(any)
          case Some(enum: Enum) =>
            Type(enum.fullName)
          case _ => any
        }
      case reference: Reference =>
        references
          .get(reference.target)
          .orElse {
            context.getEnum(reference.target)
              .map(enum => Type(enum.fullName))
          }
          .getOrElse(any)
      case condition: Condition =>
        val thenType = process(condition.thenCase)
        val elseType = process(condition.elseCase)
        if (thenType == elseType) {
          thenType
        } else {
          any
        }
    }
  }
}

object ScopedExpression {
  def apply[E <: Expression](expression: E, context: ValidationContext): ScopedExpression[E] = {
    new ScopedExpression(
      expression = expression,
      references = Map.empty,
      context = context
    )
  }

  def apply[E <: Expression](expression: E, generator: Generator, context: ValidationContext): ScopedExpression[E] = {
    new ScopedExpression(
      expression = expression,
      references = {
        generator.parameters.map { parameter =>
          if (parameter.isRest) {
            parameter.name -> Type("List", Seq(parameter.typ))
          } else {
            parameter.name -> parameter.typ
          }
        }.toMap
      },
      context = context
    )
  }

  def apply[E <: Expression](expression: E, innerScope: ScopedExpression[_]): ScopedExpression[E] = {
    new ScopedExpression(expression, innerScope.references, innerScope.context)
  }

  implicit class scopedGenerator(scopedExpression: ScopedExpression[GenerationExpression]) {
    def name: String = scopedExpression.expression.name

    def generics: Seq[Type] = scopedExpression.expression.generics

    def arguments: Seq[ScopedExpression[Expression]] = scopedExpression.expression.arguments.map(ScopedExpression(_, scopedExpression))
  }

  case class ScopedField(name: String, expression: ScopedExpression[Expression], location: Location)

  implicit class scopedStructure(scopedExpression: ScopedExpression[StructureExpression]) {
    def typ: Type = scopedExpression.expression.typ

    def fields: Seq[ScopedField] = scopedExpression.expression.fields.map(fieldToScopedField)

    private def fieldToScopedField(field: Field): ScopedField = {
      ScopedField(field.name, ScopedExpression(field.expression, scopedExpression), field.location)
    }
  }

  implicit class scopedMethodCall(scopedExpression: ScopedExpression[MethodCall]) {
    def inner: ScopedExpression[Expression] = ScopedExpression(scopedExpression.expression.inner, scopedExpression)

    def method: String = scopedExpression.expression.method

    def arguments: Seq[ScopedExpression[Expression]] = scopedExpression.expression.arguments.map(ScopedExpression(_, scopedExpression))

    def generics: Seq[Type] = scopedExpression.expression.generics
  }

  implicit class scopedAttributeCall(scopedExpression: ScopedExpression[AttributeCall]) {
    def inner: ScopedExpression[Expression] = ScopedExpression(scopedExpression.expression.inner, scopedExpression)

    def attribute: String = scopedExpression.expression.attribute
  }

  implicit class scopedReference(scopedExpression: ScopedExpression[Reference]) {
    def target: String = scopedExpression.expression.target
  }

  implicit class scopedCondition(scopedExpression: ScopedExpression[Condition]) {
    def condition: ScopedExpression[Expression] = ScopedExpression(scopedExpression.expression.condition, scopedExpression)
    def thenCase: ScopedExpression[Expression] = ScopedExpression(scopedExpression.expression.thenCase, scopedExpression)
    def elseCase: ScopedExpression[Expression] = ScopedExpression(scopedExpression.expression.elseCase, scopedExpression)
  }

}