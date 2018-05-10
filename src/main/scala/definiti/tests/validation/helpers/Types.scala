package definiti.tests.validation.helpers

import definiti.common.ast._
import definiti.tests.AST.Type

object Types {
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

  def finalType(typ: Type, library: Library): Type = {
    def process(typ: Type, previousGenericReplacement: Map[String, Type]): Type = {
      library.typesMap.get(typ.name) match {
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
            generics = typ.generics.map(finalType(_, library))
          )
      }
    }
    process(typ, Map.empty)
  }
}
