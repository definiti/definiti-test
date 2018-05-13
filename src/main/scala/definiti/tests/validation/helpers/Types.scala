package definiti.tests.validation.helpers

import definiti.common.ast.{Expression => _, _}
import definiti.tests.ast._
import definiti.tests.validation.ValidationContext

object Types {
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
