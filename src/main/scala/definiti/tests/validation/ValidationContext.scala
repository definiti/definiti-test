package definiti.tests.validation

import definiti.common.ast.{AttributeCall => _, Expression => _, MethodCall => _, Condition => _, _}
import definiti.tests.ast._
import definiti.tests.validation.helpers.ScopedExpression

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

  def extractExpressions[E <: Expression](pf: PartialFunction[Expression, E]): Seq[ScopedExpression[E]] = {
    val collector: PartialFunction[Expression, Option[E]] = pf.andThen(Some(_)).orElse { case _ => None }
    extractAllExpressions().flatMap { scopedExpression =>
      collector.apply(scopedExpression.expression)
        .map(ScopedExpression(_, scopedExpression))
    }
  }

  def extractAllExpressions(): Seq[ScopedExpression[Expression]] = {
    extractMainExpressions()
      .flatMap { scopedExpressions =>
        extractDeepExpressions(scopedExpressions.expression)
          .map(ScopedExpression(_, scopedExpressions))
      }
  }

  def extractMainExpressions(): Seq[ScopedExpression[Expression]] = {
    extractTestVerificationExpressions ++ extractTestTypeExpressions ++ extractGeneratorExpressions
  }

  def extractTestVerificationExpressions: Seq[ScopedExpression[Expression]] = {
    testVerifications
      .flatMap(_.cases)
      .flatMap(_.subCases)
      .flatMap { subCase =>
        (subCase.expression +: subCase.arguments) ++ subCase.messageArguments
      }
      .map(ScopedExpression(_, this))
  }

  def extractTestTypeExpressions: Seq[ScopedExpression[Expression]] = {
    testTypes
      .flatMap(_.cases)
      .flatMap(_.subCases)
      .flatMap { subCase =>
        (subCase.expression +: subCase.arguments) ++ subCase.messageArguments
      }
      .map(ScopedExpression(_, this))
  }

  def extractGeneratorExpressions: Seq[ScopedExpression[Expression]] = {
    context.generators.map { generator =>
      ScopedExpression(generator.expression, generator, this)
    }
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
      case condition: Condition =>
        Seq(
          Seq(condition),
          extractDeepExpressions(condition.condition),
          extractDeepExpressions(condition.thenCase),
          extractDeepExpressions(condition.elseCase)
        ).flatten
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

  def hasEnum(enumName: String): Boolean = {
    getEnum(enumName).isDefined
  }

  def hasType(typeName: String): Boolean = {
    library.typesMap.contains(typeName)
  }

  def getGenerator(generatorName: String): Option[GeneratorMeta] = {
    generators.find(_.fullName == generatorName)
  }
}