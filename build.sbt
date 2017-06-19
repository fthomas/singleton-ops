/// variables

val groupId = "eu.timepit"
val projectName = "singleton-ops"
val rootPkg = "singleton.ops"
val gitPubUrl = s"https://github.com/fthomas/$projectName.git"
val gitDevUrl = s"git@github.com:fthomas/$projectName.git"

val macroCompatVersion = "1.1.1"
val macroParadiseVersion = "2.1.0"
val shapelessVersion = "2.3.2"
val scalaCheckVersion = "1.13.4"
val scalaMetaVersion = "1.8.0"
val macroParadise3Version = "3.0.0-M9"

/// projects

lazy val root = project
  .in(file("."))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    libraryDependencies ++= Seq(
      scalaOrganization.value % "scala-compiler" % scalaVersion.value,
      "org.typelevel" %% "macro-compat" % macroCompatVersion,
      compilerPlugin("org.scalamacros" % "paradise" % macroParadiseVersion cross CrossVersion.patch),
      "com.chuusai" %% "shapeless" % shapelessVersion,
      "org.scalacheck" %% "scalacheck" % scalaCheckVersion % Test
    )
  )
  .settings( //Adds dependency of new-style scalameta and paradise macros
    libraryDependencies += "org.scalameta" %% "scalameta" % scalaMetaVersion % Provided,
    scalacOptions += "-Xplugin-require:macroparadise",
    scalacOptions in (Compile, console) := Seq(), // macroparadise plugin doesn't work in repl
    sources in (Compile,doc) := Seq.empty, // disable scaladoc due to https://github.com/scalameta/paradise/issues/55
    publishArtifact in (Compile, packageDoc) := false, // disable scaladoc
    sbt.addCompilerPlugin("org.scalameta" % "paradise" % macroParadise3Version cross CrossVersion.patch)
  )

/// settings

lazy val commonSettings = Def.settings(
  metadataSettings,
  compileSettings,
  scaladocSettings,
  releaseSettings,
  styleSettings,
  miscSettings
)

lazy val metadataSettings = Def.settings(
  name := projectName,
  organization := groupId,
  homepage := Some(url(s"https://github.com/fthomas/$projectName")),
  startYear := Some(2016),
  licenses := Seq(
    "Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
  scmInfo := Some(
    ScmInfo(homepage.value.get,
            s"scm:git:$gitPubUrl",
            Some(s"scm:git:$gitDevUrl")))
)

lazy val compileSettings = Def.settings(
  scalaOrganization := "org.typelevel",
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
    "-Xfuture",
    "-Xlint:-unused,_",
    "-Yliteral-types",
    "-Yno-adapted-args",
    "-Ywarn-numeric-widen",
    "-Ywarn-unused-import"
//    "-Ywarn-value-discard"
  ),
  scalacOptions in (Compile, console) -= "-Ywarn-unused-import",
  scalacOptions in (Test, console) -= "-Ywarn-unused-import"
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

lazy val publishSettings = Def.settings(
  publishMavenStyle := true,
  pomIncludeRepository := { _ =>
    false
  },
  pomExtra :=
    <developers>
      <developer>
        <id>fthomas</id>
        <name>Frank S. Thomas</name>
        <url>https://github.com/fthomas</url>
      </developer>
      <developer>
        <id>soronpo</id>
        <name>Oron Port</name>
        <url>https://github.com/soronpo</url>
      </developer>
    </developers>
)

lazy val noPublishSettings = Def.settings(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

lazy val releaseSettings = {
  import sbtrelease.ReleaseStateTransformations._

  lazy val updateVersionInReadme: ReleaseStep = { st: State =>
    val extracted = Project.extract(st)
    val newVersion = extracted.get(version)
    val oldVersion = "git describe --abbrev=0".!!.trim.replaceAll("^v", "")

    val readme = "README.md"
    val oldContent = IO.read(file(readme))
    val newContent = oldContent.replaceAll(oldVersion, newVersion)
    IO.write(file(readme), newContent)
    s"git add $readme" !! st.log

    st
  }

  Def.settings(
    releaseCrossBuild := true,
    releasePublishArtifactsAction := PgpKeys.publishSigned.value,
    releaseVcsSign := true,
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runClean,
      runTest,
      setReleaseVersion,
      //updateVersionInReadme,
      commitReleaseVersion,
      tagRelease,
      publishArtifacts,
      setNextVersion,
      commitNextVersion,
      pushChanges
    )
  )
}

lazy val styleSettings = Def.settings(
  //reformatOnCompileSettings
)

lazy val miscSettings = Def.settings(
  initialCommands += s"""
    import $rootPkg._
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
