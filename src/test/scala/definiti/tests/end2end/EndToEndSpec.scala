package definiti.tests.end2end

import java.nio.file.Paths

import definiti.core._
import definiti.core.ast.{Location, Root}
import definiti.core.validation.{ControlLevel, Controls}
import definiti.tests.{ConfigurationMock, ProgramResultMatchers}
import org.scalatest.{FlatSpec, Matchers}

trait EndToEndSpec extends FlatSpec with Matchers with ProgramResultMatchers {
  def processDirectory(sample: String, configuration: ConfigurationMock = ConfigurationMock()): ProgramResult[Root] = {
    val finalConfiguration = configurationDirectory(sample, configuration)
    val project = new Project(finalConfiguration)
    project.generatePublicAST().run(finalConfiguration)
  }

  def configurationDirectory(sample: String, configuration: ConfigurationMock): Configuration = {
    configuration.copy(
      source = Paths.get(s"src/test/resources/samples/${sample.replaceAll("\\.", "/")}"),
      apiSource = Paths.get(s"src/test/resources/core"),
      contexts = configuration.contexts
    )
  }

  def processFile(sample: String, configuration: ConfigurationMock = ConfigurationMock()): ProgramResult[Root] = {
    val finalConfiguration = configurationFile(sample, configuration)
    val project = new Project(finalConfiguration)
    project.generatePublicAST().run(finalConfiguration)
  }

  def configurationFile(sample: String, configuration: ConfigurationMock): Configuration = {
    configuration.copy(
      source = Paths.get(s"src/test/resources/samples/${sample.replaceAll("\\.", "/")}.def"),
      apiSource = Paths.get(s"src/test/resources/core"),
      contexts = configuration.contexts
    )
  }
}

object EndToEndSpec {

  case class LocationPath(path: String) {
    def apply(startLine: Int, startColumn: Int, endLine: Int, endColumn: Int): Location = {
      Location(path, startLine, startColumn, endLine, endColumn)
    }

    def apply(line: Int, startColumn: Int, endColumn: Int): Location = {
      Location(path, line, startColumn, line, endColumn)
    }
  }

  object LocationPath {
    def control(name: String, file: String): LocationPath = {
      LocationPath(s"src/test/resources/samples/controls/${name}/${file}.def")
    }
  }

  def configurationForceControls(controls: String*): ConfigurationMock = {
    ConfigurationMock(
      userFlags = Controls.all.map { control =>
        control.name -> (if (controls.contains(control.name)) ControlLevel.error else ControlLevel.ignored)
      }.toMap
    )
  }
}