package definiti.tests.validation.helpers

import definiti.common.ast.{TypeDeclaration, TypeReference}

object Types {
  def typeDeclarationToTypeReference(typeDeclaration: TypeDeclaration): TypeReference = {
    TypeReference(
      typeName = typeDeclaration.typeName,
      genericTypes = typeDeclaration.genericTypes.map(typeDeclarationToTypeReference)
    )
  }
}
