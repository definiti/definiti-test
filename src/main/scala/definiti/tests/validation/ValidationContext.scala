package definiti.tests.validation

import definiti.common.ast.{AttributeCall => _, Expression => _, MethodCall => _, _}
import definiti.tests.ast._

case class ValidationContext(
  context: TestsContext,
  generators: Seq[GeneratorMeta],
  library: Library
) {
  def testVerifications: Seq[TestVerification] = {
    context.tests
      .collect {
        case testVerification: TestVerification => testVerification
      }
  }

  def testTypes: Seq[TestType] = {
    context.tests
      .collect {
        case testType: TestType => testType
      }
  }

  def extractExpressions[B <: Expression](pf: PartialFunction[Expression, B]): Seq[B] = {
    extractAllExpressions().collect(pf)
  }

  def extractAllExpressions(): Seq[Expression] = {
    extractMainExpressions()
      .flatMap(extractDeepExpressions)
  }

  def extractMainExpressions(): Seq[Expression] = {
    extractTestVerificationExpressions ++ extractTestTypeExpressions ++ extractGeneratorExpressions
  }

  def extractTestVerificationExpressions: Seq[Expression] = {
    testVerifications
      .flatMap(_.cases)
      .flatMap(_.subCases)
      .flatMap { subCase =>
        (subCase.expression +: subCase.arguments) ++ subCase.messageArguments
      }
  }

  def extractTestTypeExpressions: Seq[Expression] = {
    testTypes
      .flatMap(_.cases)
      .flatMap(_.subCases)
      .flatMap { subCase =>
        (subCase.expression +: subCase.arguments) ++ subCase.messageArguments
      }
  }

  def extractGeneratorExpressions: Seq[Expression] = {
    context.generators.map(_.expression)
  }

  def extractDeepExpressions(expression: Expression): Seq[Expression] = {
    expression match {
      case generationExpression: GenerationExpression =>
        generationExpression +: generationExpression.arguments.flatMap(extractDeepExpressions)
      case structureExpression: StructureExpression =>
        structureExpression +: structureExpression.fields.map(_.expression).flatMap(extractDeepExpressions)
      case methodCall: MethodCall =>
        (methodCall +: extractDeepExpressions(methodCall.inner)) ++ methodCall.arguments.flatMap(extractDeepExpressions)
      case attributeCall: AttributeCall =>
        attributeCall +: extractDeepExpressions(attributeCall.inner)
      case other =>
        Seq(other)
    }
  }

  def getVerification(verificationName: String): Option[Verification] = {
    library.verificationsMap.get(verificationName)
  }

  def hasVerification(verificationName: String): Boolean = {
    library.verificationsMap.contains(verificationName)
  }

  def getClassDefinition(typeName: String): Option[ClassDefinition] = {
    library.typesMap.get(typeName)
  }

  def getFinalClassDefinition(typeName: String): Option[ClassDefinition] = {
    getClassDefinition(typeName) match {
      case Some(aliasType: AliasType) => getFinalClassDefinition(aliasType.alias.typeName)
      case other => other
    }
  }

  def getDefinedType(typeName: String): Option[DefinedType] = {
    library.typesMap
      .get(typeName)
      .collect {
        case definedType: DefinedType => definedType
      }
  }

  def getEnum(enumName: String): Option[ClassDefinition] = {
    library.typesMap
      .get(enumName)
      .collect {
        case enum: Enum => enum
      }
  }

  def hasType(typeName: String): Boolean = {
    library.typesMap.contains(typeName)
  }

  def getGenerator(generatorName: String): Option[GeneratorMeta] = {
    generators.find(_.fullName == generatorName)
  }
}