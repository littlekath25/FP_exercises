val scala3Version       = "3.1.1"
val scalaTestVersion    = "3.2.11"

lazy val root = project
  .in(file("."))
  .settings(
    name := "FP_excercises",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta"  %% "munit"        % "0.7.29" % Test,
    libraryDependencies += "dev.zio"        %% "zio"          % "2.0.0-M4",
    libraryDependencies += "dev.zio"        %% "zio-streams"  % "2.0.0-M4",
    libraryDependencies += "org.apache.avro" % "avro"         % "1.11.0",
    libraryDependencies += "com.sksamuel.avro4s" %% "avro4s-core" % "5.0.0",
    libraryDependencies += "org.scalatest"  %% "scalatest"     % scalaTestVersion % "test",
  )
