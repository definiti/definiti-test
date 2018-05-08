package definiti.tests

import com.typesafe.config.{Config, ConfigFactory}
import com.typesafe.scalalogging.Logger
import definiti.common.control.ControlLevel
import definiti.common.program.ProgramConfiguration
import definiti.common.utils.CollectionUtils.scalaSeq
import definiti.core.validation.Controls

case class Configuration(
  programConfiguration: ProgramConfiguration
)

private[tests] class FileConfiguration(config: Config) {
  private val logger = Logger(getClass)

  def this() {
    this(ConfigFactory.load())
  }

  def load(): Configuration = {
    Configuration(programConfiguration)
  }

  private val userFlags: Map[String, ControlLevel.Value] = extractMap("definiti.core.flags").flatMap { case (key, value) =>
    ControlLevel.fromString(value) match {
      case Some(level) =>
        Some(key -> level)
      case _ =>
        logger.warn(s"Unknown level ${value} for control ${key}, ignored")
        None
    }
  }

  private val programConfiguration: ProgramConfiguration = ProgramConfiguration(
    controlLevel = getEnumeration("definiti.core.controlLevel", ControlLevel, ControlLevel.warning),
    fatalLevel = getEnumeration("definiti.core.fatalLevel", ControlLevel, ControlLevel.error),
    userFlags = userFlags,
    defaultLevels = {
      Controls.all
        .map { control => control.name -> control.defaultLevel }
        .toMap
    }
  )

  private def extractMap(configurationKey: String): Map[String, String] = {
    val configurationMap = config.getConfig(configurationKey)
    scalaSeq(configurationMap.entrySet())
      .map(_.getKey)
      .map(key => key -> configurationMap.getString(key))
      .toMap
  }

  private def getEnumeration[A <: Enumeration](path: String, enumeration: A, defaultValue: A#Value): A#Value = {
    val stringValue = getStringOrElse(path, defaultValue.toString)
    enumeration.values.find(_.toString == stringValue) match {
      case Some(value) => value
      case None =>
        logger.warn(s"Unknown value ${stringValue} for ${path}, ignored")
        defaultValue
    }
  }

  private def getStringOrElse(configurationPath: String, defaultValue: => String): String = {
    if (config.hasPath(configurationPath)) {
      config.getString(configurationPath)
    } else {
      defaultValue
    }
  }
}