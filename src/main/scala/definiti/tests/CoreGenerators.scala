package definiti.tests

import definiti.common.validation.Validated
import definiti.tests.ast.GeneratorMeta
import definiti.tests.parser.GeneratorsContextParser

object CoreGenerators {
  private val generatorFiles: Seq[String] = {
    Seq("Boolean", "Date", "List", "Misc", "Number", "Option", "String")
      .map(file => "generators/" + file + ".gen")
  }

  lazy val coreGenerators: Validated[Seq[GeneratorMeta]] = {
    Validated.squash(generatorFiles.map(file => new GeneratorsContextParser(file).parse()))
      .map(_.flatten)
  }
}
