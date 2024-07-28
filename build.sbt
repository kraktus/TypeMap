ThisBuild / version           := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion      := "3.4.2"

// from scalachess
val commonSettings = Seq(
  scalacOptions := Seq(
    "-encoding",
    "utf-8",
    // "-source:future-migration",
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


lazy val typemap = project
  .in(file("."))
  .settings(
    commonSettings,
    name := "typemap",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )


lazy val examples = project
  .in(file("examples"))
  .settings(
    commonSettings,
  )
  .dependsOn(typemap, typemap % "compile->test")


lazy val bench = project
  .enablePlugins(JmhPlugin)
  .settings(commonSettings, name := "bench")
  .dependsOn(typemap, typemap % "compile->test")

addCommandAlias("fmt", "all typemap/scalafmtAll examples/scalafmtAll bench/scalafmtAll")