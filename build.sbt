ThisBuild / scalaVersion := "2.13.10"

lazy val compilerOptions = Seq(
  "-Ymacro-annotations",
  "-deprecation",
  "-feature",
  "-encoding", "UTF-8"
)

lazy val nativeImageSettings: Seq[Setting[_]] = Seq(
  Compile / mainClass := Some("sledger.Main"),
  nativeImageVersion := "22.3.2",
  nativeImageOptions ++= Seq(
    s"-H:DynamicProxyConfigurationFiles=${(Compile / resourceDirectory).value / "proxy-config.json"}",
    s"-H:JNIConfigurationFiles=${(Compile / resourceDirectory).value / "jni-config.json"}",
    s"-H:ReflectionConfigurationFiles=${(Compile / resourceDirectory).value / "reflect-config.json"}",
    s"-H:ResourceConfigurationFiles=${(Compile / resourceDirectory).value / "resource-config.json"}",
    "-H:+ReportExceptionStackTraces",
    //    "-H:+JNI", // not sure if need this for jna
    "--no-fallback",
  ),
  nativeImageAgentMerge := true,
  nativeImageReady := { () => () },
)

lazy val assemblySettings = Seq(
  assemblyJarName in assembly := name.value + ".jar",
  assemblyMergeStrategy in assembly := {
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case "application.conf"            => MergeStrategy.concat
    case x =>
      val oldStrategy = (assemblyMergeStrategy in assembly).value
      oldStrategy(x)
  }
)
lazy val commonSettings = Seq(
  scalacOptions ++= compilerOptions,
  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "fansi" % "0.4.0",
    "com.lihaoyi" %% "pprint" % "0.8.1",
    "org.typelevel" %% "cats-effect" % "3.4.8",
    "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test,
    compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
  ),
  addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)
)

lazy val `sledger-cli` = project
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      "org.jline" % "jline" % "3.23.0",
      "org.jline" % "jline-terminal-jna" % "3.23.0",
      "com.monovore" %% "decline-effect" % "2.4.1",
    ),
    assemblySettings,
    nativeImageSettings,
    assembly / mainClass := Some("sledger.Main"),
  )
  .enablePlugins(NativeImagePlugin)
  .enablePlugins(AssemblyPlugin)
  .dependsOn(`sledger-lib`)


lazy val `sledger-lib` = project
  .settings(
    name := "sledger-lib",
    commonSettings,
    libraryDependencies ++= Seq(
      "org.jline" % "jline" % "3.23.0",
      "org.scala-lang.modules" %% "scala-collection-contrib" % "0.3.0",
      "com.github.j-mie6" %% "parsley" % "4.2.9",
      "com.github.j-mie6" %% "parsley-cats" % "1.2.0",
      "org.typelevel" %% "kittens" % "3.0.0",
      "io.estatico" %% "newtype" % "0.4.4",
      "org.apache.commons" % "commons-text" % "1.10.0"
    ),
  )
  .disablePlugins(AssemblyPlugin)

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala-ledger",
    publish := {}, publish / skip := true,
  )
  .disablePlugins(AssemblyPlugin)
  .aggregate(`sledger-lib`, `sledger-cli`)

