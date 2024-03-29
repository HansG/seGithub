import Dependencies._
import sbt.Keys.scalacOptions

ThisBuild / scalaVersion := "2.13.8"
ThisBuild / version := "2.0.0"
ThisBuild / organization := "dev.profunktor"
ThisBuild / organizationName := "ProfunKtor"

ThisBuild / evictionErrorLevel := Level.Warn
ThisBuild / scalafixDependencies += Libraries.organizeImports

resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += Resolver.mavenCentral
resolvers += "mvnrepository" at "https://mvnrepository.com/artifact"
//resolvers += "Local Maven Repository" at "file://D:/se/m2/repository"
resolvers += "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository"

def dep(org: String, prefix: String, version: String)(modules: String*)(testModules: String*) =
  modules.map(m => org       %% (prefix ++ m) % version) ++
    testModules.map(m => org %% (prefix ++ m) % version) //% Test

addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.13.2" cross CrossVersion.full)
addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1")

val scalafixCommonSettings = inConfig(IntegrationTest)(scalafixConfigSettings(IntegrationTest))
lazy val root = (project in file("."))
  .settings(
    name := "shopping-cart"
  )
  .aggregate(core, tests)

lazy val tests = (project in file("modules/tests"))
  .configs(IntegrationTest)
  .settings(
    name := "shopping-cart-test-suite",
    scalacOptions ++= List("-Ymacro-annotations", "-Yrangepos", "-Wconf:cat=unused:info"),
    testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
    testFrameworks += new TestFramework("munit.Framework"),
    Defaults.itSettings,
    scalafixCommonSettings,
    libraryDependencies ++= Seq(
      CompilerPlugin.kindProjector,
      CompilerPlugin.betterMonadicFor,
      CompilerPlugin.semanticDB,
      Libraries.catsLaws,
      Libraries.log4catsNoOp,
      Libraries.monocleLaw,
      Libraries.refinedScalacheck,
      Libraries.weaverCats,
      Libraries.weaverDiscipline,
      Libraries.logback % Runtime,
      Libraries.weaverScalaCheck,
     // "io.github.quafadas" %% "scautable" % "0.0.5",
      "org.scalameta" %% "munit" % "0.7.29",
      "org.scalatest" %% "scalatest" % "3.2.15"
    ) ++
      dep("org.typelevel", "cats-effect", "3.3.12")("")("-laws", "-testkit") ++
      dep("org.scalameta", "munit", "0.7.29")()("", "-scalacheck") ++
      dep("org.typelevel", "", "1.0.7")()("munit-cats-effect-3") ++
      dep("org.typelevel", "scalacheck-effect", "1.0.3")()("", "-munit")
  )
  .dependsOn(core)

lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")

val natchezVersion = "0.1.6"
lazy val core = (project in file("modules/core"))
  .enablePlugins(DockerPlugin)
  .enablePlugins(AshScriptPlugin)
  .settings(
    name := "shopping-cart-core",
    Docker / packageName := "shopping-cart",
    scalacOptions ++= List(
      "-Ymacro-annotations",
      "-Yrangepos",
      "-Wconf:cat=unused:info",
      "-language:higherKinds",
      "-Ydelambdafy:inline"
    ),
    scalafmtOnCompile := true,
    resolvers += Resolver.sonatypeRepo("snapshots"),
    Defaults.itSettings,
    scalafixCommonSettings,
    dockerBaseImage := "openjdk:11-jre-slim-buster",
    dockerExposedPorts ++= Seq(8080),
    makeBatScripts := Seq(),
    dockerUpdateLatest := true,
    libraryDependencies ++= Seq(
      CompilerPlugin.kindProjector,
      CompilerPlugin.betterMonadicFor,
      CompilerPlugin.semanticDB,
      Libraries.cats,
      Libraries.catsEffect,
      Libraries.catsRetry,
      Libraries.circeCore,
      Libraries.circeGeneric,
      Libraries.circeParser,
      Libraries.circeRefined,
      Libraries.cirisCore,
      Libraries.cirisEnum,
      Libraries.cirisRefined,
      Libraries.derevoCore,
      Libraries.derevoCats,
      Libraries.derevoCirce,
      Libraries.fs2,
      Libraries.fs2io,
      Libraries.http4sDsl,
      Libraries.http4sServer,
      Libraries.http4sClient,
      Libraries.http4sCirce,
      Libraries.http4sJwtAuth,
      Libraries.javaxCrypto,
      Libraries.log4cats,
      Libraries.logback % Runtime,
      Libraries.monocleCore,
      Libraries.newtype,
      Libraries.redis4catsEffects,
      Libraries.redis4catsLog4cats,
      Libraries.refinedCore,
      Libraries.refinedCats,
      Libraries.skunkCore,
      Libraries.skunkCirce,
      Libraries.squants,
      // "com.lihaoyi" %% "ammonite" % "2.5.3" cross CrossVersion.full
      "com.lihaoyi"        %% "ammonite"            % "2.5.4" cross CrossVersion.full,
      "com.github.scopt"   %% "scopt"               % "4.1.0",
      "org.tpolecat"       %% "natchez-core"        % natchezVersion,
      "org.tpolecat"       %% "natchez-jaeger"      % natchezVersion,
      "org.tpolecat"       %% "natchez-honeycomb"   % natchezVersion,
      "io.github.kirill5k" %% "mongo4cats-core"     % "0.6.10",
      "io.github.kirill5k" %% "mongo4cats-circe" % "0.6.10",
      "io.github.kirill5k" %% "mongo4cats-embedded" % "0.6.10",
      Libraries.catsLaws,
      Libraries.log4catsNoOp,
      Libraries.monocleLaw,
      Libraries.refinedScalacheck,
      Libraries.weaverCats,
      Libraries.weaverDiscipline,
      Libraries.logback % Runtime,
      Libraries.weaverScalaCheck
    ),
    libraryDependencies ++= javaFXModules.map(m => "org.openjfx" % s"javafx-$m" % "11" classifier "win")
      ++
        dep("org.typelevel", "cats-effect", "3.3.12")("")("-laws", "-testkit") ++
      dep("org.scalameta", "munit", "0.7.29")()("", "-scalacheck") ++
      dep("org.typelevel", "", "1.0.7")()("munit-cats-effect-3") ++
      dep("org.typelevel", "scalacheck-effect", "1.0.3")()("", "-munit")
  )

addCommandAlias("runLinter", ";scalafixAll --rules OrganizeImports")

(fullClasspath in Runtime) ++= {
  (updateClassifiers in Runtime).value.configurations
    .find(_.configuration.name == Test.name)
    .get
    .modules
    .flatMap(_.artifacts)
    .collect { case (a, f) if a.classifier == Some("sources") => f }
}
/*
fullClasspath  ++= {
  updateClassifiers.value
    .configurations
    .flatMap(_.modules)
    .flatMap(_.artifacts)
    .collect{case (a, f) if a.classifier == Some("sources") => f}
}
 */
