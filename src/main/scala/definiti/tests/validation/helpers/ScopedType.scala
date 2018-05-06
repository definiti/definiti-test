package definiti.tests.validation.helpers

import definiti.core.ast.{AbstractTypeReference, TypeReference, Verification}
import definiti.tests.AST._

case class ScopedType(typeReference: AbstractTypeReference, generics: Seq[String]) {
  def isSameAs(typ: Type): Boolean = {
    typeReference match {
      case typeReference: TypeReference => isSameAstypeReference(typ, typeReference)
      case _ => false
    }
  }

  private def isSameAstypeReference(typ: Type, typeReference: TypeReference): Boolean = {
    if (generics.contains(typeReference.typeName)) {
      true
    } else {
      def sameTypeName = typ.name == typeReference.typeName

      def sameNumberOfGenerics = typ.generics.length == typeReference.genericTypes.length

      def sameGenerics = typ.generics.zip(typeReference.genericTypes).forall { case (genericType, genericTypeReference) =>
        isSameAstypeReference(genericType, genericTypeReference)
      }

      sameTypeName && sameNumberOfGenerics && sameGenerics
    }
  }
}

object ScopedType {
  def apply(typeReference: AbstractTypeReference, verification: Verification): ScopedType = {
    new ScopedType(typeReference, verification.function.genericTypes)
  }

  def apply(typeReference: AbstractTypeReference): ScopedType = {
    new ScopedType(typeReference, Seq.empty)
  }
}