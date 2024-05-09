
import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._
//import scalariform.formatter.preferences._
//import com.typesafe.sbt.SbtScalariform.ScalariformKeys

ThisBuild / organization := "org.openmole"
name := "byte-pack"

ThisBuild / scalaVersion := "3.3.3"

ThisBuild / licenses := Seq("Affero GPLv3" -> url("http://www.gnu.org/licenses/"))
ThisBuild / homepage := Some(url("https://github.com/openmole/byte-pack"))

ThisBuild / publishTo := sonatypePublishToBundle.value

ThisBuild / pomIncludeRepository := { _ => false}
ThisBuild / scmInfo := Some(ScmInfo(url("https://github.com/openmole/byte-pack.git"), "scm:git:git@github.com:openmole/byte-pack.git"))

ThisBuild / pomExtra := {
  <!-- Developer contact information -->
    <developers>
      <developer>
        <id>romainreuillon</id>
        <name>Romain Reuillon</name>
        <url>https://github.com/romainreuillon/</url>
      </developer>
    </developers>
}


releaseVersionBump := sbtrelease.Version.Bump.Minor

releaseTagComment    := s"Releasing ${(ThisBuild / version).value}"

releaseCommitMessage := s"Bump version to ${(ThisBuild / version).value}"

sonatypeProfileName := "org.openmole"


releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  tagRelease,
  releaseStepCommandAndRemaining("+publishSigned"),
  releaseStepCommand("sonatypeBundleRelease"),
  setNextVersion,
  commitNextVersion,
  //releaseStepCommand("sonatypeReleaseAll"),
  pushChanges
)

def settings = Seq (
  resolvers += Resolver.sonatypeRepo("snapshots"),
  scalacOptions ++= Seq("-Xtarget:11", "-language:postfixOps")
)

lazy val bytePack =
  Project(id = "byte-pack", base = file("byte-pack")) settings(
    settings,
    libraryDependencies += "io.github.bishabosha" %% "enum-extensions" % "0.1.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % Test
  )

