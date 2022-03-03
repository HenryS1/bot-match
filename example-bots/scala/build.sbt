val scala3Version = "3.1.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.15.0-M1",
      "io.circe" %% "circe-generic" % "0.15.0-M1",
      "io.circe" %% "circe-parser" % "0.15.0-M1",
      "io.circe" %% "circe-fs2" % "0.14.0",
      "org.typelevel" %% "cats-core" % "2.7.0",
      "org.typelevel" %% "cats-effect" % "3.3.5",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.13.5",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.13.5" % "compile-internal",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-circe" % "2.13.5",
      "co.fs2" %% "fs2-core" % "3.2.5",
      "co.fs2" %% "fs2-io" % "3.2.5",
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )

  )
