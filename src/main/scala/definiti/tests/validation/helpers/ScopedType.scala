package definiti.tests.validation.helpers

import definiti.common.ast._
import definiti.tests.ast._

case class ScopedType(typeReference: AbstractTypeReference, generics: Seq[String]) {
  def isSameAs(typ: Type): Boolean = {
    typeReference match {
      case typeReference: TypeReference => isSameAsTypeReference(typ, typeReference)
      case _ => false
    }
  }

  private def isSameAsTypeReference(typ: Type, typeReference: TypeReference): Boolean = {
    if (generics.contains(typeReference.typeName)) {
      true
    } else {
      def sameTypeName = typ.name == typeReference.typeName

      def sameNumberOfGenerics = typ.generics.length == typeReference.genericTypes.length

      def sameGenerics = typ.generics.zip(typeReference.genericTypes).forall { case (genericType, genericTypeReference) =>
        isSameAsTypeReference(genericType, genericTypeReference)
      }

      sameTypeName && sameNumberOfGenerics && sameGenerics
    }
  }
}

object ScopedType {
  def apply(typeReference: AbstractTypeReference, verification: Verification): ScopedType = {
    new ScopedType(typeReference, verification.function.genericTypes)
  }

  def apply(typeReference: AbstractTypeReference, definedType: DefinedType): ScopedType = {
    new ScopedType(typeReference, definedType.genericTypes)
  }

  def apply(typeDeclaration: TypeDeclaration, definedType: DefinedType): ScopedType = {
    new ScopedType(Types.typeDeclarationToTypeReference(typeDeclaration), definedType.genericTypes)
  }

  def apply(typeReference: AbstractTypeReference): ScopedType = {
    new ScopedType(typeReference, Seq.empty)
  }

  def apply(typeReference: AbstractTypeReference, methodDefinition: MethodDefinition): ScopedType = {
    new ScopedType(typeReference, methodDefinition.genericTypes)
  }
}