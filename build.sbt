ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.16"


val Http4sVersion = "0.23.23"
val CirceVersion = "0.14.6"
val LogbackVersion = "1.4.11"
val CatsParseVersion = "0.3.10"

lazy val root = (project in file("."))
  .settings(
    name := "chat-app",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "3.3.0",
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-ember-server" % Http4sVersion,
      "org.http4s" %% "http4s-circe" % Http4sVersion,
      "io.circe" %% "circe-generic" % CirceVersion,
      "org.http4s" %% "http4s-dsl" % Http4sVersion,
      "org.typelevel" %% "cats-parse" % CatsParseVersion,
      "ch.qos.logback" % "logback-classic" % LogbackVersion,
    )
  )
