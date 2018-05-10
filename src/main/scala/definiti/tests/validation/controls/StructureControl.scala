package definiti.tests.validation.controls

import definiti.common.ast._
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.tests.AST.{ConstructorExpression, StructureExpression, TestsContext, Type, Expression}
import definiti.tests.validation.helpers.{ExpressionTypes, ScopedType, Types}

object StructureControl extends Control[TestsContext] {
  override def description: String = "Control if structure expression matches the expected type"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(context: TestsContext, library: Library): ControlResult = {
    extractStructures(context).map(controlStructure(_, library))
  }

  private def extractStructures(context: TestsContext): Seq[StructureExpression] = {
    (context.testVerifications.flatMap(_.cases) ++ context.testTypes.flatMap(_.cases))
      .flatMap(_.subCases)
      .map(_.expression)
      .flatMap(extractStructuresFromExpression)
  }

  private def extractStructuresFromExpression(expression: Expression): Seq[StructureExpression] = {
    expression match {
      case constructorExpression: ConstructorExpression =>
        constructorExpression.arguments.flatMap(extractStructuresFromExpression)
      case structureExpression: StructureExpression =>
        structureExpression +: structureExpression.fields.map(_.expression).flatMap(extractStructuresFromExpression)
      case _ =>
        Seq.empty
    }
  }

  private def controlStructure(structureExpression: StructureExpression, library: Library): ControlResult = {
    library.definedTypes.find(_.fullName == structureExpression.typ.name) match {
      case Some(definedType) =>
        ControlResult.squash {
          Seq(
            controlFieldList(structureExpression, definedType),
            controlFieldTypes(structureExpression, definedType)
          )
        }
      case None =>
        ignored
    }
  }

  private def controlFieldList(structureExpression: StructureExpression, definedType: DefinedType): ControlResult = {
    val addedFields = structureExpression.fields
      .filter(field => definedType.attributes.forall(_.name != field.name))
      .map(field => fieldAdded(field.name, structureExpression.typ, field.location))

    val missingFields = definedType.attributes
      .filter(attribute => structureExpression.fields.forall(_.name != attribute.name))
      .map(field => fieldMissing(field.name, structureExpression.typ, structureExpression.location))

    (addedFields ++ missingFields).map(ControlResult(_))
  }

  private def controlFieldTypes(structureExpression: StructureExpression, definedType: DefinedType): ControlResult = {
    val fieldsWithAttributes = structureExpression.fields.flatMap { field =>
      definedType.attributes
        .find(_.name == field.name)
        .map(attribute => field -> attribute)
    }
    fieldsWithAttributes.map { case (field, attribute) =>
      val fieldType = ExpressionTypes.getTypeOfExpression(field.expression)
      val attributeType = attribute.typeDeclaration
      if (ScopedType(attributeType, definedType).isSameAs(fieldType)) {
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
