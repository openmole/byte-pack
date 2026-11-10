
ThisBuild / organization := "org.openmole"
name := "byte-pack"

ThisBuild / scalaVersion := "3.9.0"

ThisBuild / licenses := Seq("Affero GPLv3" -> url("http://www.gnu.org/licenses/"))
ThisBuild / homepage := Some(url("https://github.com/openmole/byte-pack"))

def settings = Seq()

lazy val bytePackType =
  Project(id = "byte-pack-type", base = file("byte-pack-types")) settings(
    settings,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % Test
  )

lazy val bytePack =
  Project(id = "byte-pack", base = file("byte-pack")) dependsOn bytePackType settings(
    settings,
    libraryDependencies += "io.github.bishabosha" %% "enum-extensions" % "0.1.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % Test
  )

lazy val bytePackMonocle =
  Project(id = "byte-pack-monocle", base = file("byte-pack-monocle")) dependsOn (bytePack) settings(
    settings,
    libraryDependencies += "dev.optics" %% "monocle-core"  % "3.2.0",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % Test
  )
