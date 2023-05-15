ThisBuild / organization := "com.truedrog"
ThisBuild / scalaVersion := "2.13.10"
assembly / mainClass := Some("com.truedrog.sledger.Main")

lazy val compilerOptions = Seq(
  "-Ymacro-annotations",
  "-deprecation",
  "-feature",
  "-encoding", "UTF-8"
)

lazy val commonSettings = Seq(
  scalacOptions ++= compilerOptions,
  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "fansi" % "0.4.0",
    "com.lihaoyi" %% "pprint" % "0.8.1",
    "org.typelevel" %% "cats-effect" % "3.4.8",
    "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test,
    "org.typelevel" %% "cats-effect-kernel" % "3.4.7",
    "org.typelevel" %% "cats-effect-std" % "3.4.7",
    compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
  ),
  addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)
)

lazy val `sledger-cli` = project
  .dependsOn(`sledger-lib`)
  .settings(
    assemblySettings,
    commonSettings,
    logLevel := util.Level.Debug,
    libraryDependencies ++= Seq(
      "org.jline" % "jline" % "3.14.0",
      "org.jline" % "jline-terminal-jna" % "3.14.0",
      "com.monovore" %% "decline" % "2.4.1",
      "com.monovore" %% "decline-effect" % "2.4.1",
    ),
  )

lazy val `sledger-lib` = project
  .settings(
    assemblySettings,
    name := "sledger-lib",
    commonSettings,
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-collection-contrib" % "0.3.0",
      "com.github.j-mie6" %% "parsley" % "4.2.9",
      "com.github.j-mie6" %% "parsley-cats" % "1.2.0",
      "org.typelevel" %% "kittens" % "3.0.0",
      "io.estatico" %% "newtype" % "0.4.4",
      "io.github.akiomik" %% "seaw" % "0.1.0",
      "org.apache.commons" % "commons-text" % "1.10.0"
    ),
  )

lazy val root = project
  .aggregate(`sledger-lib`, `sledger-cli`)
  .in(file("."))
  .disablePlugins(AssemblyPlugin)
  .settings(
    name := "scala-ledger",
    publish := {}, publish / skip := true,
  )

lazy val assemblySettings = Seq(
  assemblyMergeStrategy in assembly := {
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case x =>
      val oldStrategy = (assemblyMergeStrategy in assembly).value
      oldStrategy(x)
  }
)