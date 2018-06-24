package definiti.tests.validation.helpers

import definiti.common.ast._
import definiti.tests.ast._
import definiti.tests.validation.ValidationContext

case class ScopedType(typeReference: AbstractTypeReference, generics: Seq[String], context: ValidationContext) {
  def isSameAs(typ: Type): Boolean = {
    typeReference match {
      case typeReference: TypeReference =>
        isSameAs(
          typ = Types.finalType(typ, context),
          reference = Types.finalType(Types.typeReferenceToType(typeReference), context)
        )
      case _ => false
    }
  }

  private def isSameAs(typ: Type, reference: Type): Boolean = {
    if (generics.contains(reference.name)) {
      true
    } else {
      def sameTypeName = typ.name == reference.name

      def sameNumberOfGenerics = typ.generics.length == reference.generics.length

      def sameGenerics = typ.generics.zip(reference.generics).forall {
        case (genericType, genericTypeReference) => isSameAs(genericType, genericTypeReference)
      }

      sameTypeName && sameNumberOfGenerics && sameGenerics
    }
  }
}

object ScopedType {
  def apply(typeReference: AbstractTypeReference, verification: Verification, context: ValidationContext): ScopedType = {
    new ScopedType(typeReference, verification.function.genericTypes, context)
  }

  def apply(typeReference: AbstractTypeReference, definedType: DefinedType, context: ValidationContext): ScopedType = {
    new ScopedType(typeReference, definedType.genericTypes, context)
  }

  def apply(typeDeclaration: TypeDeclaration, definedType: DefinedType, context: ValidationContext): ScopedType = {
    new ScopedType(Types.typeDeclarationToTypeReference(typeDeclaration), definedType.genericTypes, context)
  }

  def apply(typeReference: AbstractTypeReference, context: ValidationContext): ScopedType = {
    new ScopedType(typeReference, Seq.empty, context)
  }

  def apply(typeReference: AbstractTypeReference, methodDefinition: MethodDefinition, context: ValidationContext): ScopedType = {
    new ScopedType(typeReference, methodDefinition.genericTypes, context)
  }
}