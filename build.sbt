name := "singleton-ops"

scalaVersion := "2.11.8"
scalaOrganization := "org.typelevel"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yliteral-types",
  "-Yno-adapted-args",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard"
)

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "org.typelevel" %% "macro-compat" % "1.1.1",
  compilerPlugin(
    "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.scalacheck" %% "scalacheck" % "1.13.2" % "test"
)

/// settings

lazy val noPublishSettings = Def.settings(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

lazy val styleSettings = Def.settings(
  reformatOnCompileSettings
)

lazy val miscSettings = Def.settings(
  initialCommands += """
    import singleton.ops._
  """
)

/// commands

val validateCommands = Seq(
  "clean",
  //"scalafmtTest",
  "coverage",
  "test",
  "coverageReport",
  "coverageOff",
  "doc"
)
addCommandAlias("validate", validateCommands.mkString(";", ";", ""))
