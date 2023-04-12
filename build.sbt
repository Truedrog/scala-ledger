ThisBuild / organization := "com.example"
ThisBuild / scalaVersion := "2.13.5"
scalacOptions ++= Seq("-Ymacro-annotations", "-deprecation", "-feature")

lazy val root = (project in file(".")).settings(
  name := "scala-ledger",
  libraryDependencies ++= Seq(
    // "core" module - IO, IOApp, schedulers
    // This pulls in the kernel and std modules automatically.
    "org.typelevel" %% "cats-effect" % "3.3.12",
    // concurrency abstractions and primitives (Concurrent, Sync, Async etc.)
    "org.typelevel" %% "cats-effect-kernel" % "3.3.12",
    // standard "effect" library (Queues, Console, Random etc.)
    "org.typelevel" %% "cats-effect-std" % "3.3.12",
    // better monadic for compiler plugin as suggested by documentation
    compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test,
    "com.olvind.tui" %% "tui" % "0.0.5",

    "com.github.j-mie6" %% "parsley" % "4.2.9",
    "com.github.j-mie6" %% "parsley-cats" % "1.2.0",
    "org.typelevel" %% "kittens" % "3.0.0",
    "io.estatico" %% "newtype" % "0.4.4"
  ),
  addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)
)
