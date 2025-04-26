ThisBuild / version           := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion      := "3.6.4"
ThisBuild / organization  := "org.lichess"

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
    name := "examples"
  )
  .dependsOn(typemap, typemap % "compile->test")


lazy val bench = project
  .enablePlugins(JmhPlugin)
  .settings(commonSettings, name := "bench")
  .dependsOn(typemap, typemap % "compile->test")

val toAllProjects = (cmd: String) => s"all typemap/$cmd examples/$cmd bench/$cmd"

addCommandAlias("build", toAllProjects("compile"))
addCommandAlias("fmt", toAllProjects("scalafmtAll"))
addCommandAlias("fmtCheck", toAllProjects("scalafmtCheckAll"))
addCommandAlias("test", toAllProjects("test"))