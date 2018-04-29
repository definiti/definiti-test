import sbt.url

organization := "io.github.definiti"

name := "tests"

version := "0.3.0-SNAPSHOT"

scalaVersion := "2.12.5"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "io.github.definiti" %% "core" % "0.3.0-SNAPSHOT"
libraryDependencies += "org.antlr" % "antlr4" % "4.7.1"
libraryDependencies += "io.spray" %%  "spray-json" % "1.3.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.5" % "test"
libraryDependencies += "io.github.definiti" % "api" % "0.3.0-SNAPSHOT" % "test"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-language:implicitConversions", "-feature")

lazy val antlr = TaskKey[Unit]("antlr", "Build Antlr files")

lazy val classpathSeparator =
  if (sys.props("os.name").toLowerCase.contains("windows")) ";"
  else ":"

antlr := {
  import scala.sys.process._

  val log = streams.value.log
  val classpath = (dependencyClasspath in Compile).value.files.mkString(classpathSeparator)
  val mainClass = "org.antlr.v4.Tool"
  val destination = "src/main/java/definiti/tests/parser/antlr"
  val packageName = "definiti.tests.parser.antlr"
  val source = "src/main/antlr/Tests.g4"

  val command = s"""java -cp "$classpath" $mainClass -o $destination -package $packageName $source"""

  log.info("Building antlr Definiti files")
  command.!
}

compile in Compile := (compile in Compile).dependsOn(antlr).value

useGpg := true

pomIncludeRepository := { _ => false }

licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

homepage := Some(url("https://definiti.github.io"))

scmInfo := Some(
  ScmInfo(
    url("https://github.com/definiti/definiti-tests"),
    "scm:git@github.com:definiti/definiti-tests.git"
  )
)

developers := List(
  Developer(
    id = "grizio",
    name = "Gaëtan Rizio",
    email = "gaetan@rizio.fr",
    url = url("https://github.com/grizio")
  )
)

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}