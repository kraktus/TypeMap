inThisBuild(
  Seq(
    scalaVersion  := "3.7.0",
    versionScheme := Some("early-semver"),
    version       := "0.2.2",
    organization  := "org.lichess",
    licenses += ("MIT" -> url("https://opensource.org/licenses/MIT")),
    publishTo     := Option(Resolver.file("file", new File(sys.props.getOrElse("publishTo", ""))))
  )
)

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

lazy val typemap = project
  .in(file("."))
  .settings(
    commonSettings,
    name                                   := "typemap",
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

