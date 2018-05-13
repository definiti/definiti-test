package definiti.tests.parser

import definiti.common.ast.Location
import definiti.common.utils.StringUtils
import definiti.tests.ast.{TestsContext => TestsContextAST, _}
import definiti.tests.parser.antlr.TestsParser._
import definiti.tests.utils.CollectionUtils
import org.antlr.v4.runtime.tree.TerminalNode

import scala.collection.mutable.ListBuffer

class AntlrToAstAdapter(packageName: String, imports: Map[String, String], val location: Location) extends LocationUtils {
  def toAST(context: TestsContext): TestsContextAST = {
    val tests = ListBuffer[Test]()
    val generators = ListBuffer[Generator]()

    CollectionUtils.scalaSeq(context.toplevel()).foreach { element =>
      appendIfDefined(element.testVerification(), tests, processTestVerification)
      appendIfDefined(element.testType(), tests, processTestType)
      appendIfDefined(element.generator(), generators, processGenerator)
    }

    TestsContextAST(
      tests = List(tests: _*),
      generators = List(generators: _*)
    )
  }

  private def appendIfDefined[A, B](element: A, buffer: ListBuffer[B], transformer: A => B): Unit = {
    if (element != null) {
      buffer.append(transformer(element))
    }
  }

  private def processTestVerification(context: TestVerificationContext): TestVerification = {
    TestVerification(
      verification = nameWithImport(context.IDENTIFIER().getText),
      cases = CollectionUtils.scalaSeq(context.testCase()).map(processTestCase),
      comment = extractDocComment(context.DOC_COMMENT()),
      location = getLocationFromContext(context)
    )
  }

  private def processTestType(context: TestTypeContext): TestType = {
    TestType(
      typ = processType(context.`type`()),
      cases = CollectionUtils.scalaSeq(context.testCase()).map(processTestCase),
      comment = extractDocComment(context.DOC_COMMENT()),
      location = getLocationFromContext(context)
    )
  }

  private def processTestCase(context: TestCaseContext): Case = {
    Case(
      kind = CaseKind.withName(context.kind.getText),
      subCases = CollectionUtils.scalaSeq(context.testSubCase()).map(processTestSubCase),
      comment = extractDocComment(context.DOC_COMMENT()),
      location = getLocationFromContext(context)
    )
  }

  private def processExpression(context: ExpressionContext): Expression = {
    val location = getLocationFromContext(context)
    val booleanOpt = Option(context.BOOLEAN()).map(boolean => BooleanExpression(boolean.getText == "true", location))
    val numberOpt = Option(context.NUMBER()).map(number => NumberExpression(BigDecimal(number.getText), location))
    val stringOpt = Option(context.STRING()).map(string => StringExpression(string.getText, location))
    val generationOpt = Option(context.generation()).map(processGeneration)
    val structureOpt = Option(context.structure()).map(processStructure)
    val methodOpt = Option(context).filter(_.method != null).map(processMethod)
    val attributeOpt = Option(context).filter(_.attribute != null).map(processAttribute)
    val referenceOpt = Option(context).filter(_.reference != null).map(processReference)

    booleanOpt
      .orElse(numberOpt)
      .orElse(stringOpt)
      .orElse(generationOpt)
      .orElse(structureOpt)
      .orElse(methodOpt)
      .orElse(attributeOpt)
      .orElse(referenceOpt)
      .getOrElse {
        // Should not happen because all cases have been processed.
        // Defensive coding when adding types.
        throw new RuntimeException(s"Unknown expression ${context}")
      }
  }

  private def processGeneration(context: GenerationContext): Expression = {
    GenerationExpression(
      name = nameWithImport(context.name.getText),
      generics = Option(context.generics()).map(processGenerics).getOrElse(Seq.empty),
      arguments = processArguments(context.arguments()),
      location = getLocationFromContext(context)
    )
  }

  private def processType(context: TypeContext): Type = {
    Type(
      name = nameWithImport(context.IDENTIFIER().getText),
      generics = Option(context.generics()).map(processGenerics).getOrElse(Seq.empty)
    )
  }

  private def processGenerics(context: GenericsContext): Seq[Type] = {
    CollectionUtils.scalaSeq(context.`type`()).map(processType)
  }

  private def processStructure(context: StructureContext): Expression = {
    StructureExpression(
      typ = processType(context.`type`()),
      fields = CollectionUtils.scalaSeq(context.field()).map(processField),
      location = getLocationFromContext(context)
    )
  }

  private def processField(context: FieldContext): Field = {
    Field(
      name = context.IDENTIFIER().getText,
      expression = processExpression(context.expression()),
      location = getLocationFromContext(context)
    )
  }

  private def processMethod(context: ExpressionContext): MethodCall = {
    MethodCall(
      inner = processExpression(context.inner),
      method = context.method.getText,
      generics = Option(context.generics()).map(processGenerics).getOrElse(Seq.empty),
      arguments = processArguments(context.arguments()),
      location = getLocationFromContext(context)
    )
  }

  private def processAttribute(context: ExpressionContext): AttributeCall = {
    AttributeCall(
      inner = processExpression(context.inner),
      attribute = context.attribute.getText,
      location = getLocationFromContext(context)
    )
  }

  private def processReference(context: ExpressionContext): Reference = {
    Reference(
      target = nameWithImport(context.reference.getText),
      location = getLocationFromContext(context)
    )
  }

  private def processTestSubCase(context: TestSubCaseContext): SubCase = {
    SubCase(
      expression = processExpression(context.expression()),
      arguments = Option(context.withArguments).map(processArguments).getOrElse(Seq.empty),
      messageArguments = Option(context.asArguments).map(processArguments).getOrElse(Seq.empty),
      location = getLocationFromContext(context)
    )
  }

  private def processArguments(context: ArgumentsContext): Seq[Expression] = {
    CollectionUtils.scalaSeq(context.expression()).map(processExpression)
  }

  private def processGenerator(context: GeneratorContext): Generator = {
    val name = context.name.getText
    Generator(
      name = name,
      fullName = StringUtils.canonical(packageName, name),
      generics = Option(context.rawGenerics()).map(processRawGenerics).getOrElse(Seq.empty),
      typ = processType(context.`type`()),
      parameters = processParameters(context.parameters()),
      expression = processExpression(context.expression()),
      location = getLocationFromContext(context)
    )
  }

  private def processRawGenerics(context: RawGenericsContext): Seq[String] = {
    CollectionUtils.scalaSeq(context.IDENTIFIER()).map(_.getText)
  }

  private def processParameters(context: ParametersContext): Seq[Parameter] = {
    CollectionUtils.scalaSeq(context.parameter()).map(processParameter)
  }

  private def processParameter(context: ParameterContext): Parameter = {
    Parameter(
      name = context.IDENTIFIER().getText,
      typ = processType(context.`type`()),
      isRest = false,
      location = getLocationFromContext(context)
    )
  }

  private def extractDocComment(node: TerminalNode): Option[String] = {
    Option(node).map(_.getText).map { content =>
      var temporaryResult = content
      if (temporaryResult.startsWith("/**")) {
        temporaryResult = temporaryResult.substring(3)
      }
      if (temporaryResult.endsWith("*/")) {
        temporaryResult = temporaryResult.substring(0, temporaryResult.length - 2)
      }
      temporaryResult
    }
  }

  private def nameWithImport(name: String): String = {
    imports.getOrElse(name, name)
  }
}
