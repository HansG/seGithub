import Dependencies._

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
resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"



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
      Libraries.weaverScalaCheck
    )
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
    scalacOptions ++= List("-Ymacro-annotations", "-Yrangepos", "-Wconf:cat=unused:info"),
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
      "com.lihaoyi" %% "ammonite" % "2.5.4" cross CrossVersion.full,
      "com.github.scopt" %% "scopt" % "4.1.0",
      "org.tpolecat" %% "natchez-core" % natchezVersion,
      "org.tpolecat" %% "natchez-jaeger" % natchezVersion,
      "org.tpolecat" %% "natchez-honeycomb" % natchezVersion,
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
  )

addCommandAlias("runLinter", ";scalafixAll --rules OrganizeImports")


(fullClasspath in Runtime) ++= {
  (updateClassifiers in Runtime).value
  .configurations
  .find(_.configuration.name == Test.name)
  .get
  .modules
  .flatMap(_.artifacts)
  .collect{case (a, f) if a.classifier == Some("sources") => f}
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