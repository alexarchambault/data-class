inThisBuild(
  List(
    organization := "io.github.alexarchambault",
    homepage := Some(url("https://github.com/alexarchambault/data-class")),
    licenses := List(
      "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
    ),
    developers := List(
      Developer(
        "alexarchambault",
        "Alexandre Archambault",
        "",
        url("https://github.com/alexarchambault")
      )
    )
  )
)

lazy val `data-class` = project
  .in(file("."))
  .settings(
    Settings.shared,
    libraryDependencies ++= Seq(
      Deps.scalaReflect.value,
      Deps.shapeless % Test,
      Deps.utest.value % Test
    ),
    testFrameworks += new TestFramework("utest.runner.Framework")
  )

lazy val `test-proj-v1` = project
  .in(file("test/proj-v1"))
  .dependsOn(`data-class` % Provided)
  .settings(
    Settings.shared,
    organization := "dataclass.test",
    moduleName := "proj",
    version := "1.0"
  )

lazy val `test-proj-v2` = project
  .in(file("test/proj-v2"))
  .dependsOn(`data-class` % Provided)
  .settings(
    Settings.shared,
    organization := "dataclass.test",
    moduleName := "proj",
    version := "2.0",
    mimaPreviousArtifacts := Set(
      organization.value %% moduleName.value % version.in(`test-proj-v1`).value
    )
  )

lazy val `proj-v1-user` = project
  .in(file("test/user-proj-v1"))
  .dependsOn(`test-proj-v1`)
  .settings(
    Settings.shared
  )
