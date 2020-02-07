import scala.sys.process._
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

/// variables

val groupId = "eu.timepit"
val projectName = "singleton-ops"
val rootPkg = "singleton"
val gitPubUrl = s"https://github.com/fthomas/$projectName.git"
val gitDevUrl = s"git@github.com:fthomas/$projectName.git"

val macroParadiseVersion = "2.1.1"
val shapelessVersion = "2.3.3"
val scalaCheckVersion = "1.14.3"

/// projects
lazy val root = project.in(file("."))
  .settings(commonSettings)
  .aggregate(singleton_opsJVM, singleton_opsJS)
  .settings(
    skip in publish := true,
    sources in Compile := Seq.empty,
    sources in Test := Seq.empty
  )

lazy val singleton_ops = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("."))
  .settings(commonSettings)
  .jsSettings(
    coverageEnabled := false
  )

lazy val singleton_opsJVM = singleton_ops.jvm
lazy val singleton_opsJS  = singleton_ops.js

/// settings

lazy val commonSettings = Def.settings(
  metadataSettings,
  compileSettings,
  scaladocSettings,
  miscSettings
)

lazy val metadataSettings = Def.settings(
  name := projectName,
  organization := groupId,
  homepage := Some(url(s"https://github.com/fthomas/$projectName")),
  startYear := Some(2016),
  licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
  scmInfo := Some(ScmInfo(homepage.value.get, s"scm:git:$gitPubUrl", Some(s"scm:git:$gitDevUrl"))),
  developers := List(
    Developer("fthomas", "Frank S. Thomas", "", url("https://github.com/fthomas")),
    Developer("soronpo", "Oron Port", "", url("https://github.com/soronpo"))
  )
)

lazy val compileSettings = Def.settings(
  scalaOrganization := "org.scala-lang",
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
//    "-Xlint:-unused,_",
//    "-Yliteral-types",
    "-Ywarn-numeric-widen"
//    "-Ywarn-value-discard"
  ),
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v >= 13 =>
        Nil
      case _ =>
        Seq(
          "-Yno-adapted-args",
          "-Ywarn-unused-import",
          "-Xplugin-require:macroparadise"
        )
    }
  },
  scalacOptions in (Compile, console) -= "-Ywarn-unused-import",
  scalacOptions in (Test, console) -= "-Ywarn-unused-import",
  libraryDependencies ++= Seq(
    scalaOrganization.value % "scala-compiler" % scalaVersion.value,
    "com.chuusai" %%% "shapeless" % shapelessVersion,
    "org.scalacheck" %%% "scalacheck" % scalaCheckVersion % Test
  ),
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      // if scala 2.13+ is used, macro annotations are merged into scala-reflect
      // https://github.com/scala/scala/pull/6606
      case Some((2, v)) if v >= 13 =>
        Seq()
      case _ =>
        Seq(
          compilerPlugin("org.scalamacros" % "paradise" % macroParadiseVersion cross CrossVersion.patch)
        )
    }
  }
)

lazy val scaladocSettings = Def.settings(
  scalacOptions in (Compile, doc) ++= Seq(
    "-doc-source-url",
    scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath",
    baseDirectory.in(LocalRootProject).value.getAbsolutePath
  ),
  autoAPIMappings := true
)

lazy val miscSettings = Def.settings(
  initialCommands += s"""
    import $rootPkg.ops._
    import $rootPkg.twoface._
  """
)

/// commands

val validateCommands = Seq(
  "clean",
  "test:compile",
  "singleton_opsJS/test",
  "coverage",
  "singleton_opsJVM/test",
  "coverageReport",
  "coverageOff",
  "doc"
)
addCommandAlias("validate", validateCommands.mkString(";", ";", ""))
