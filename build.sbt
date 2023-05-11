ThisBuild / organization := "com.example"
ThisBuild / scalaVersion := "2.13.10"

lazy val `sledger-lib` = project
  .settings(
    name := "sledger-lib",
    scalacOptions ++= Seq(
      "-Ymacro-annotations",
      "-deprecation",
      "-feature",
      "-encoding", "UTF-8"
    ),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.3.12",
      "org.typelevel" %% "cats-effect-kernel" % "3.3.12",
      "org.typelevel" %% "cats-effect-std" % "3.3.12",
      compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
      "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test,
      "com.olvind.tui" %% "tui" % "0.0.5",
      "org.scala-lang.modules" %% "scala-collection-contrib" % "0.3.0",
      "com.github.j-mie6" %% "parsley" % "4.2.9",
      "com.github.j-mie6" %% "parsley-cats" % "1.2.0",
      "org.typelevel" %% "kittens" % "3.0.0",
      "io.estatico" %% "newtype" % "0.4.4",
      "io.github.akiomik" %% "seaw" % "0.1.0",
      "org.apache.commons" % "commons-text" % "1.10.0"
    ),
    addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)
  )
lazy val root = project
  .aggregate(`sledger-lib`)
  .in(file("."))
  .settings(
    name := "scala-ledger",
    scalacOptions ++= Seq(
      "-Ymacro-annotations",
      "-deprecation",
      "-feature",
      "-encoding", "UTF-8"
    )
  )
