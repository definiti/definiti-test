package definiti.tests

import definiti.common.control.{Control, ControlLevel}
import definiti.common.tests.ConfigurationMock
import definiti.tests.validation.controls.Controls

case class ConfigurationBuilder(configurationMock: ConfigurationMock = ConfigurationMock()) {
  def withOnlyControls(controls: String*): ConfigurationBuilder = {
    ConfigurationBuilder(configurationMock.copy(
      userFlags = Controls.all.map { control =>
        control.name -> (if (controls.contains(control.name)) ControlLevel.error else ControlLevel.ignored)
      }.toMap
    ))
  }

  def withOnlyControls(controls: Control[_]*)(implicit dummyImplicit: DummyImplicit): ConfigurationBuilder = {
    withOnlyControls(controls.map(_.name): _*)
  }

  def build(): ConfigurationMock = {
    configurationMock.copy(
      contexts = configurationMock.contexts :+ buildPlugin()
    )
  }

  private def buildPlugin(): TestsPlugin = new TestsPlugin() {
    override val configuration = Configuration(configurationMock.programConfiguration)
  }
}

object ConfigurationBuilder {
  implicit def configurationBuilderToConfigurationMock(configurationBuilder: ConfigurationBuilder): ConfigurationMock = {
    configurationBuilder.build()
  }
}