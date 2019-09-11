ThisBuild / organization := "scytest"
ThisBuild / scalaVersion := "2.12.9"

ThisBuild / resolvers += Resolver.bintrayRepo("colisweb", "maven")

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
      "co.fs2" %% "fs2-core" % "2.0.0",
      "io.chrisdavenport" %% "cats-par" % "0.2.1",
      "com.chuusai" %% "shapeless" % "2.3.3",
      "io.opentracing.brave" % "brave-opentracing" % "0.34.2",
      "com.colisweb" %% "scala-opentracing" % "0.0.6",
      "io.zipkin.reporter2" % "zipkin-sender-urlconnection" % "2.10.0",
      "org.typelevel" %% "cats-tagless-macros" % "0.9"
    )
  )

lazy val root = (project in file(".")).dependsOn(scytest).aggregate(scytest)
