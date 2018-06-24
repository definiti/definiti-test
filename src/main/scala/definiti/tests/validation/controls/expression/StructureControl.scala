package definiti.tests.validation.controls.expression

import definiti.common.ast.{Expression => _, _}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.ast._
import definiti.tests.validation.ValidationContext
import definiti.tests.validation.helpers.{ScopedExpression, ScopedType, Types}

object StructureControl extends Control[ValidationContext] {
  override def description: String = "Control if structure expression matches the expected type"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: ValidationContext, library: Library): ControlResult = {
    context
      .extractExpressions { case structureExpression: StructureExpression => structureExpression }
      .map(controlStructure(_, context))
  }

  private def controlStructure(structureExpression: ScopedExpression[StructureExpression], context: ValidationContext): ControlResult = {
    context.getDefinedType(structureExpression.typ.name) match {
      case Some(definedType) =>
        ControlResult.squash {
          Seq(
            controlFieldList(structureExpression, definedType),
            controlFieldTypes(structureExpression, definedType, context)
          )
        }
      case None =>
        ignored
    }
  }

  private def controlFieldList(structureExpression: ScopedExpression[StructureExpression], definedType: DefinedType): ControlResult = {
    val addedFields = structureExpression.fields
      .filter(field => definedType.attributes.forall(_.name != field.name))
      .map(field => fieldAdded(field.name, structureExpression.typ, field.location))

    val missingFields = definedType.attributes
      .filter(attribute => structureExpression.fields.forall(_.name != attribute.name))
      .map(field => fieldMissing(field.name, structureExpression.typ, structureExpression.location))

    (addedFields ++ missingFields).map(ControlResult(_))
  }

  private def controlFieldTypes(structureExpression: ScopedExpression[StructureExpression], definedType: DefinedType, context: ValidationContext): ControlResult = {
    val fieldsWithAttributes = structureExpression.fields.flatMap { field =>
      definedType.attributes
        .find(_.name == field.name)
        .map(attribute => field -> attribute)
    }
    fieldsWithAttributes.map { case (field, attribute) =>
      val fieldType = field.expression.typeOfExpression
      val attributeType = attribute.typeDeclaration
      if (ScopedType(attributeType, definedType, context).isSameAs(fieldType)) {
        ControlResult.OK
      } else {
        ControlResult(invalidType(Types.typeDeclarationToTypeReference(attributeType), fieldType, field.expression.location))
      }
    }
  }

  def fieldMissing(field: String, typ: Type, location: Location): Alert = {
    alert(s"The field ${field} is missing from type ${typ.readableString}", location)
  }

  def fieldAdded(field: String, typ: Type, location: Location): Alert = {
    alert(s"The field ${field} does not exist in type ${typ.readableString}", location)
  }

  def invalidType(expected: TypeReference, got: Type, location: Location): Alert = {
    alert(s"Expected type ${expected.readableString}, got ${got.readableString}", location)
  }
}
