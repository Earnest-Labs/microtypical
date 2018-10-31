name := "com.earnest"
organization := "microtypical"
scalaVersion := "2.12.7"
scalacOptions ++= Seq (
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xfuture",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ypartial-unification",
  "-Ywarn-macros:after",
  "-Ywarn-unused-import",
  "-Ywarn-unused:explicits,-implicits",
  "-Ywarn-value-discard",
)
scalacOptions in (Compile, console) := scalacOptions.value filterNot Set (
  "-Xfatal-warnings",
  "-Xlint",
  "-Ywarn-unused-import",
)
scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value
scalacOptions in Tut := (scalacOptions in (Compile, console)).value
updateOptions := updateOptions.value withCachedResolution true
conflictManager := ConflictManager.latestRevision // this is the default, but we set it explicitly for IDEA. See: https://youtrack.jetbrains.com/issue/SCL-7646
doctestTestFramework := DoctestTestFramework.ScalaTest
cancelable in Global := true
parallelExecution in Test := false

lazy val libVersions = new Object {
  val circe = "0.10.0"
}

lazy val libraries = Seq (
  "com.twitter" %% "util-core" % "18.9.1",
  "io.circe" %% "circe-core" % libVersions.circe,
  "io.circe" %% "circe-generic" % libVersions.circe,
  "io.circe" %% "circe-parser" % libVersions.circe,
  "org.scalacheck" %% "scalacheck" % "1.13.5",
  "org.typelevel" %% "cats-core" % "1.4.0",
  "org.typelevel" %% "cats-effect" % "1.1.0-M1",
  "org.typelevel" %% "cats-mtl-core" % "0.4.0",
)

lazy val testLibraries = Seq (
  "org.scalatest" %% "scalatest" % "3.0.5",
)

libraryDependencies ++= libraries
libraryDependencies ++= testLibraries map (_ % Test)

addCompilerPlugin ("org.spire-math" %% "kind-projector" % "0.9.3")

enablePlugins (MicrositesPlugin)
enablePlugins (TutPlugin)

micrositeBaseUrl := ""
micrositeName := "Earnest Scala Standard Utils"
micrositeAuthor := "Earnest"
micrositeGithubOwner := "meetearnest"
micrositeGithubRepo := "microtypical"
micrositeHomepage := "https://meetearnest.com/"
