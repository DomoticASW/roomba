val scala3Version = "3.7.0"

enablePlugins(JavaAppPackaging)
enablePlugins(DockerPlugin)

packageName := "ventus218/domoticasw-roomba"

lazy val root = project
  .in(file("."))
  .settings(
    name := "roomba",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version
  )
