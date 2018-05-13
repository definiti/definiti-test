package definiti.tests.parser

import definiti.common.ast.Location
import definiti.common.utils.StringUtils
import definiti.common.validation.{Invalid, Valid, Validated}
import definiti.tests.AST.{TestsContext => TestsContextAST, _}
import definiti.tests.parser.antlr.TestsParser._
import definiti.tests.parser.antlr.{TestsLexer, TestsParser}
import definiti.tests.utils.CollectionUtils
import org.antlr.v4.runtime.tree.TerminalNode
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

import scala.collection.mutable.ListBuffer

class TestsContextParser(packageName: String, imports: Map[String, String], val location: Location) extends LocationUtils {
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

  private def toAST(testsContext: TestsContext): TestsContextAST = {
    val internalImports = ListBuffer[(String, String)]()
    CollectionUtils.scalaSeq(testsContext.toplevel()).foreach { topLevel =>
      Option(topLevel.generator()).foreach { generator =>
        val name = generator.IDENTIFIER().getText
        val fullName = StringUtils.canonical(packageName, name)
        internalImports.append(name -> fullName)
      }
    }
    new AntlrToAstAdapter(
      packageName = packageName,
      imports = imports ++ internalImports.toMap,
      location = location
    ).toAST(testsContext)
  }
}
