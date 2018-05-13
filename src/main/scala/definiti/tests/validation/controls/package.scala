package definiti.tests.validation

import definiti.common.ast.{ClassDefinition, DefinedType, Verification}
import definiti.tests.AST._

package object controls {

  implicit class ValidationContextExtension(validationContext: ValidationContext) {
    def testVerifications: Seq[TestVerification] = {
      validationContext.context.testVerifications
    }

    def testTypes: Seq[TestType] = {
      validationContext.context.testTypes
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
      validationContext.context.generators.map(_.expression)
    }

    def extractDeepExpressions(expression: Expression): Seq[Expression] = {
      expression match {
        case generationExpression: GenerationExpression =>
          generationExpression +: generationExpression.arguments.flatMap(extractDeepExpressions)
        case structureExpression: StructureExpression =>
          structureExpression +: structureExpression.fields.map(_.expression).flatMap(extractDeepExpressions)
        case other =>
          Seq(other)
      }
    }

    def getVerification(verificationName: String): Option[Verification] = {
      validationContext.library.verificationsMap.get(verificationName)
    }

    def hasVerification(verificationName: String): Boolean = {
      validationContext.library.verificationsMap.contains(verificationName)
    }

    def getClassDefinition(typeName: String): Option[ClassDefinition] = {
      validationContext.library.typesMap.get(typeName)
    }

    def getDefinedType(typeName: String): Option[DefinedType] = {
      validationContext.library.typesMap
        .get(typeName)
        .collect {
          case definedType: DefinedType => definedType
        }
    }

    def hasType(typeName: String): Boolean = {
      validationContext.library.typesMap.contains(typeName)
    }

    def getGenerator(generatorName: String): Option[GeneratorMeta] = {
      validationContext.generators.find(_.fullName == generatorName)
    }
  }

  implicit class TestsContextExtension(testsContext: TestsContext) {
    def testVerifications: Seq[TestVerification] = {
      testsContext.tests
        .collect {
          case testVerification: TestVerification => testVerification
        }
    }

    def testTypes: Seq[TestType] = {
      testsContext.tests
        .collect {
          case testType: TestType => testType
        }
    }
  }
}
