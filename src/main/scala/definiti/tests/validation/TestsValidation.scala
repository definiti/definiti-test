package definiti.tests.validation

import definiti.common.ast.{ExtendedContext, Library}
import definiti.common.control.ControlResult
import definiti.tests.Configuration
import definiti.tests.ast.{Generator, GeneratorMeta, TestsContext}

class TestsValidation(library: Library, configuration: Configuration, coreGenerators: Seq[GeneratorMeta]) {
  def validate(context: TestsContext): ControlResult = {
    val validationContext = buildValidationContext(context)
    Controls.all
      .filter(configuration.programConfiguration.isControlAccepted)
      .map(_.control(validationContext, library))
  }

  private def buildValidationContext(context: TestsContext): ValidationContext = {
    val projectGenerators = library.root.namespaces
      .flatMap(_.elements)
      .collect {
        case context: ExtendedContext[TestsContext] if context.name == "tests" => context
      }
      .map(_.content)
      .flatMap(_.generators)
      .map(generatorToGeneratorMeta)

    ValidationContext(context, coreGenerators ++ projectGenerators, library)
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
