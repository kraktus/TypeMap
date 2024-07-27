val scala3Version = "3.4.2"

// from scalachess
val commonSettings = Seq(
  scalacOptions := Seq(
    "-encoding",
    "utf-8",
    "-source:future-migration",
    "-indent",
    "-feature",
    // "-language:postfixOps",
    "-Wunused:all",
    "-release:21"
    // "-Werror"
    // Warnings as errors!
    /* "-Xfatal-warnings" */
  )
)


lazy val root = project
  .in(file("."))
  .settings(
    commonSettings,
    name := "typemap",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )
