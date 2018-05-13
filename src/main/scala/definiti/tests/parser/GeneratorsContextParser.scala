package definiti.tests.parser

import java.nio.file.{Files, Paths}

import definiti.common.ast.Location
import definiti.common.validation.{Invalid, Valid, Validated}
import definiti.tests.ast._
import definiti.tests.parser.antlr.GeneratorsParser._
import definiti.tests.parser.antlr.{GeneratorsLexer, GeneratorsParser}
import definiti.tests.utils.CollectionUtils
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

class GeneratorsContextParser(file: String) extends LocationUtils {
  def location = Location(file, 1, 1, 0, 0)

  def parse(): Validated[Seq[GeneratorMeta]] = {
    Valid(file)
      .map(getClass.getClassLoader.getResource)
      .map(url => CollectionUtils.scalaSeq(Files.lines(Paths.get(url.toURI))).mkString("\n"))
      .flatMap(toAntlr)
      .map(toAST)
  }

  def toAntlr(content: String): Validated[GeneratorsContext] = {
    val errorListener = new ErrorListener(location)
    val parser = buildAntlrParser(content, errorListener)
    val result: GeneratorsContext = parser.generators()
    if (errorListener.hasError) {
      Invalid(errorListener.errors.map(_.toError))
    } else {
      Valid(result)
    }
  }

  private def buildAntlrParser(content: String, errorListener: ErrorListener): GeneratorsParser = {
    val lexer = new GeneratorsLexer(CharStreams.fromString(content))
    val tokens = new CommonTokenStream(lexer)
    val parser = new GeneratorsParser(tokens)
    parser.removeErrorListeners()
    parser.addErrorListener(errorListener)
    parser
  }

  private def toAST(context: GeneratorsContext): Seq[GeneratorMeta] = {
    CollectionUtils.scalaSeq(context.generator())
      .map(processGenerator)
  }

  private def processGenerator(context: GeneratorContext): GeneratorMeta = {
    GeneratorMeta(
      fullName = context.name.getText,
      generics = Option(context.generics()).map(processGenerics).getOrElse(Seq.empty),
      typ = processType(context.`type`()),
      parameters = processParameters(context.parameters())
    )
  }

  private def processGenerics(context: GenericsContext): Seq[String] = {
    CollectionUtils.scalaSeq(context.IDENTIFIER()).map(_.getText)
  }

  private def processType(context: TypeContext): Type = {
    Type(
      name = context.IDENTIFIER().getText,
      generics = Option(context.genericTypes()).map(processGenericTypes).getOrElse(Seq.empty)
    )
  }

  private def processGenericTypes(context: GenericTypesContext): Seq[Type] = {
    CollectionUtils.scalaSeq(context.`type`()).map(processType)
  }

  private def processParameters(context: ParametersContext): Seq[Parameter] = {
    CollectionUtils.scalaSeq(context.parameter()).map(processParameter)
  }

  private def processParameter(context: ParameterContext): Parameter = {
    Parameter(
      name = context.IDENTIFIER().getText,
      typ = processType(context.`type`()),
      isRest = Option(context.restSymbol).isDefined,
      location = getLocationFromContext(context)
    )
  }
}
