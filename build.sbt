val scala3Version = "3.1.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "FP_excercises",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta"  %% "munit"        % "0.7.29" % Test,
    libraryDependencies += "dev.zio"        %% "zio"          % "2.0.0-M4",
    libraryDependencies += "dev.zio"        %% "zio-streams"  % "2.0.0-M4"
  )
