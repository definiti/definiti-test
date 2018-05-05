package definiti.tests.parser

import definiti.core.ast.Location
import definiti.core.{Invalid, Valid, Validated}
import definiti.tests.AST.{TestsContext => TestsContextAST, _}
import definiti.tests.parser.antlr.TestsParser._
import definiti.tests.parser.antlr.{TestsLexer, TestsParser}
import definiti.tests.utils.CollectionUtils
import org.antlr.v4.runtime.tree.TerminalNode
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

import scala.collection.mutable.ListBuffer

class TestsContextParser(val location: Location) extends LocationUtils {
  def parse(content: String): Validated[TestsContextAST] = {
    Valid(content)
      .flatMap(toAntlr)
      .map(toAST)
  }

  def toAntlr(content: String): Validated[TestsContext] = {
    val errorListener = new ErrorListener(location)
    val parser = buildAntlrParser(content, errorListener)
    val result: TestsContext = parser.tests()
    if (errorListener.hasError) {
      Invalid(errorListener.errors.map(_.toError))
    } else {
      Valid(result)
    }
  }

  private def buildAntlrParser(content: String, errorListener: ErrorListener): TestsParser = {
    val lexer = new TestsLexer(CharStreams.fromString(content))
    val tokens = new CommonTokenStream(lexer)
    val parser = new TestsParser(tokens)
    parser.removeErrorListeners()
    parser.addErrorListener(errorListener)
    parser
  }

  private def toAST(context: TestsContext): TestsContextAST = {
    val testVerifications = ListBuffer[TestVerification]()

    CollectionUtils.scalaSeq(context.toplevel()).foreach { element =>
      appendIfDefined(element.testVerification(), testVerifications, processTestVerification)
    }

    TestsContextAST(
      tests = List(testVerifications: _*)
    )
  }

  private def appendIfDefined[A, B](element: A, buffer: ListBuffer[B], transformer: A => B): Unit = {
    if (element != null) {
      buffer.append(transformer(element))
    }
  }

  private def processTestVerification(context: TestVerificationContext): TestVerification = {
    TestVerification(
      verification = context.IDENTIFIER().getText,
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

    booleanOpt
      .orElse(numberOpt)
      .orElse(stringOpt)
      .getOrElse {
        // Should not happen because all cases have been processed.
        // Defensive coding when adding types.
        throw new RuntimeException(s"Unknown expression ${context}")
      }
  }

  private def processTestSubCase(context: TestSubCaseContext): SubCase = {
    SubCase(
      expression = processExpression(context.expression()),
      arguments = Option(context.arguments()).map(processArguments).getOrElse(Seq.empty),
      location = getLocationFromContext(context)
    )
  }

  private def processArguments(context: ArgumentsContext): Seq[Expression] = {
    CollectionUtils.scalaSeq(context.expression()).map(processExpression)
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
}
