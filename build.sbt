ThisBuild / organization := "scytest"
ThisBuild / scalaVersion := "2.12.8"

lazy val commonSettings = Seq(
  addCompilerPlugin(
    "org.typelevel" % "kind-projector" % "0.10.3" cross CrossVersion.binary
  ),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
)

lazy val scytest = (project in file("scytest"))
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-core" % "1.1.0-M1",
      "io.chrisdavenport" %% "cats-par" % "0.2.1"
    )
  )

lazy val root = (project in file(".")).dependsOn(scytest).aggregate(scytest)
