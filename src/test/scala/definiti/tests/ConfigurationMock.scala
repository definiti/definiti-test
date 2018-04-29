package definiti.tests

import java.nio.file.{Path, Paths}

import definiti.core.{ContextPlugin, GeneratorPlugin, ParserPlugin, ValidatorPlugin}
import definiti.core.validation.ControlLevel
import definiti.core.Configuration

case class ConfigurationMock(
  source: Path = Paths.get(""),
  apiSource: Path = Paths.get(""),
  parsers: Seq[ParserPlugin] = Seq.empty,
  validators: Seq[ValidatorPlugin] = Seq.empty,
  generators: Seq[GeneratorPlugin] = Seq.empty,
  contexts: Seq[ContextPlugin[_]] = Seq(new TestsPlugin),
  controlLevel: ControlLevel.Value = ControlLevel.warning,
  fatalLevel: ControlLevel.Value = ControlLevel.error,
  userFlags: Map[String, ControlLevel.Value] = Map.empty
) extends Configuration