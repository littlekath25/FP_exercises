val scala3Version       = "3.1.1"
val scalaTestVersion    = "3.2.11"

lazy val root = project
  .in(file("."))
  .settings(
    name := "FP_excercises",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta"        %% "munit"        % "0.7.29" % Test,
    libraryDependencies += "org.scalatest"        %% "scalatest"    % scalaTestVersion % "test",
    libraryDependencies += "com.typesafe.play"    %% "play-json"    % "2.10.0-RC5"
  )
