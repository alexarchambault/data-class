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
