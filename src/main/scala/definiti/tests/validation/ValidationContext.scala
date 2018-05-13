package definiti.tests.validation

import definiti.common.ast.{ExtendedContext, Library}
import definiti.tests.AST._

case class ValidationContext(
  context: TestsContext,
  generators: Seq[GeneratorMeta],
  library: Library
)

object ValidationContext {
  def apply(
    context: TestsContext,
    library: Library,
    coreGenerators: Seq[GeneratorMeta]
  ): ValidationContext = {
    val projectGenerators = library.root.namespaces
      .flatMap(_.elements)
      .collect {
        case context: ExtendedContext[TestsContext] if context.name == "tests" => context
      }
      .map(_.content)
      .flatMap(_.generators)
      .map(generatorToGeneratorMeta)

    new ValidationContext(context, coreGenerators ++ projectGenerators, library)
  }

  private def generatorToGeneratorMeta(generator: Generator): GeneratorMeta = {
    GeneratorMeta(
      fullName = generator.fullName,
      generics = generator.generics,
      typ = generator.typ,
      parameters = generator.parameters
    )
  }
}

case class GeneratorMeta(
  fullName: String,
  generics: Seq[String],
  typ: Type,
  parameters: Seq[Parameter]
)