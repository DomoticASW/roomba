val scala3Version = "3.7.0"

lazy val root = project
  .in(file("."))
  .enablePlugins(JavaAppPackaging, DockerPlugin)
  .settings(
    name := "roomba",
    version := sys.env.getOrElse("VERSION", "0.1.0-SNAPSHOT"),
    scalaVersion := scala3Version,
    scalacOptions += "-deprecation",
    Docker / packageName := "ventus218/domoticasw-roomba",
    dockerUpdateLatest := true
  )
