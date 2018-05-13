package definiti.tests.validation.helpers

import definiti.common.ast.{AttributeCall => _, Expression => _, MethodCall => _, Reference => _, _}
import definiti.tests.ast._
import definiti.tests.validation.ValidationContext

object Types {
  private val boolean = Type("Boolean", Seq.empty)
  private val number = Type("Number", Seq.empty)
  private val string = Type("String", Seq.empty)
  private val any = Type("Any", Seq.empty)

  def getTypeOfExpression(expression: Expression, context: ValidationContext): Type = {
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
        val innerExpressionType = getTypeOfExpression(methodCall.inner, context)
        context.getFinalClassDefinition(innerExpressionType.name) match {
          case Some(native: NativeClassDefinition) =>
            (for {
              method <- native.methods.find(_.name == methodCall.method)
              returnType = typeReferenceToType(method.returnType)
              finalType = Types.replaceGenerics(returnType, method.genericTypes.zip(methodCall.generics).toMap)
            } yield {
              finalType
            })
              .getOrElse(any)
          case _ => any
        }
      case attributeCall: AttributeCall =>
        val innerExpressionType = getTypeOfExpression(attributeCall.inner, context)
        context.getFinalClassDefinition(innerExpressionType.name) match {
          case Some(native: NativeClassDefinition) =>
            native.attributes
              .find(_.name == attributeCall.attribute)
              .map(_.typeDeclaration)
              .map(typeDeclarationToType)
              .getOrElse(any)
          case Some(definedType: DefinedType) =>
            definedType.attributes
              .find(_.name == attributeCall.attribute)
              .map(_.typeDeclaration)
              .map(typeDeclarationToType)
              .getOrElse(any)
          case Some(enum: Enum) =>
            Type(enum.fullName)
          case _ => any
        }
      case reference: Reference =>
        // For now, only enum are accepted as targets.
        context.getEnum(reference.target)
          .map(enum => Type(enum.fullName))
          .getOrElse(any)
    }
  }

  def typeDeclarationToTypeReference(typeDeclaration: TypeDeclaration): TypeReference = {
    TypeReference(
      typeName = typeDeclaration.typeName,
      genericTypes = typeDeclaration.genericTypes.map(typeDeclarationToTypeReference)
    )
  }

  def typeDeclarationToType(typeDeclaration: TypeDeclaration): Type = {
    Type(
      name = typeDeclaration.typeName,
      generics = typeDeclaration.genericTypes.map(typeDeclarationToType)
    )
  }

  def typeReferenceToType(typeReference: TypeReference): Type = {
    Type(
      name = typeReference.typeName,
      generics = typeReference.genericTypes.map(typeReferenceToType)
    )
  }

  def finalType(typ: Type, context: ValidationContext): Type = {
    def process(typ: Type, previousGenericReplacement: Map[String, Type]): Type = {
      context.library.typesMap.get(typ.name) match {
        case Some(aliasType: AliasType) =>
          val newGenericReplacement = aliasType.genericTypes.zip(typ.generics).toMap
          val genericReplacements = previousGenericReplacement ++ newGenericReplacement
          val typeWithReplacedGenerics = Type(
            name = aliasType.alias.typeName,
            generics = aliasType.alias.genericTypes.map { typeDeclaration =>
              genericReplacements.getOrElse(typeDeclaration.typeName, typeDeclarationToType(typeDeclaration))
            }
          )
          process(typeWithReplacedGenerics, genericReplacements)
        case _ =>
          Type(
            name = typ.name,
            generics = typ.generics.map(finalType(_, context))
          )
      }
    }
    process(typ, Map.empty)
  }

  def replaceGenerics(typ: Type, genericReplacement: Map[String, Type]): Type = {
    genericReplacement.get(typ.name) match {
      case Some(typeReplacement) => typeReplacement
      case None => typ.copy(
        generics = typ.generics.map(replaceGenerics(_, genericReplacement))
      )
    }
  }
}
