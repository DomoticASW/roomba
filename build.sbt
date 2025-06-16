val scala3Version = "3.7.0"

lazy val root = project
  .in(file("."))
  .enablePlugins(JavaAppPackaging, DockerPlugin)
  .settings(
    name := "roomba",
    version := sys.env.getOrElse("VERSION", "0.1.0-SNAPSHOT"),
    scalaVersion := scala3Version,
    scalacOptions += "-deprecation",
    fork := true,
    libraryDependencies += "com.lihaoyi" %% "upickle" % "4.2.1",
    libraryDependencies += "com.softwaremill.sttp.client4" %% "core" % "4.0.0-RC1",
    libraryDependencies += "org.apache.pekko" %% "pekko-http" % "1.2.0",
    libraryDependencies += "org.apache.pekko" %% "pekko-http-spray-json" % "1.2.0",
    libraryDependencies += "org.apache.pekko" %% "pekko-stream" % "1.1.3",
    libraryDependencies += "org.apache.pekko" %% "pekko-actor-typed" % "1.1.3",
    Docker / packageName := "ventus218/domoticasw-roomba",
    dockerBaseImage := "openjdk:21",
    dockerExposedPorts += 8080,
    dockerUpdateLatest := true
  )
