import sbt.url

organization := "io.github.definiti"

name := "tests"

scalaVersion := "2.12.6"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "io.github.definiti" %% "core" % "0.3.0"
libraryDependencies += "org.antlr" % "antlr4" % "4.7.1"
libraryDependencies += "io.spray" %%  "spray-json" % "1.3.4"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.2.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
libraryDependencies += "io.github.definiti" % "api" % "0.3.0" % "test"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-language:implicitConversions", "-feature")

lazy val antlr = TaskKey[Unit]("antlr", "Build Antlr files")

lazy val antlrTests = TaskKey[Unit]("antlrTests", "Build Antlr files for tests context")

lazy val antlrGenerators = TaskKey[Unit]("antlrGenerators", "Build Antlr files for generators")

lazy val classpathSeparator =
  if (sys.props("os.name").toLowerCase.contains("windows")) ";"
  else ":"

antlrTests := {
  import scala.sys.process._

  val log = streams.value.log
  val classpath = (dependencyClasspath in Compile).value.files.mkString(classpathSeparator)
  val mainClass = "org.antlr.v4.Tool"
  val destination = "src/main/java/definiti/tests/parser/antlr"
  val packageName = "definiti.tests.parser.antlr"
  val source = "src/main/antlr/Tests.g4"

  val command = s"""java -cp "$classpath" $mainClass -o $destination -package $packageName $source"""

  log.info("Building antlr tests files")
  command.!
}

antlrGenerators := {
  import scala.sys.process._

  val log = streams.value.log
  val classpath = (dependencyClasspath in Compile).value.files.mkString(classpathSeparator)
  val mainClass = "org.antlr.v4.Tool"
  val destination = "src/main/java/definiti/tests/parser/antlr"
  val packageName = "definiti.tests.parser.antlr"
  val source = "src/main/antlr/Generators.g4"

  val command = s"""java -cp "$classpath" $mainClass -o $destination -package $packageName $source"""

  log.info("Building antlr generators files")
  command.!
}

antlr := {}

antlr := antlr.dependsOn(antlrTests, antlrGenerators).value

compile in Compile := (compile in Compile).dependsOn(antlr).value

releasePublishArtifactsAction := PgpKeys.publishSigned.value

pomIncludeRepository := { _ => false }

licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

homepage := Some(url("https://definiti.gitbook.io/definiti"))

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