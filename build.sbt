name := "singleton-ops"

scalaVersion := "2.11.8"
scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
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
  compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.scalacheck" %% "scalacheck" % "1.13.2" % "test"
)

initialCommands += """
  import singleton.ops._
"""

reformatOnCompileSettings

addCommandAlias("validate", ";clean;coverage;test;coverageReport;coverageOff")
