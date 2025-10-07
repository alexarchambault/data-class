import sbt._
import sbt.Keys._

object Settings {

  def scala213 = "2.13.17"
  def scala212 = "2.12.20"

  private lazy val isScala212 = Def.setting {
    scalaVersion.value.startsWith("2.12.")
  }

  lazy val shared = Def.settings(
    scalaVersion := scala213,
    crossScalaVersions := Seq(scala213, scala212),
    libraryDependencies ++= {
      if (isScala212.value)
        Seq(
          compilerPlugin(
            ("org.scalamacros" % "paradise" % "2.1.1").cross(CrossVersion.full)
          )
        )
      else
        Nil
    },
    scalacOptions ++= {
      if (isScala212.value) Nil
      else List("-Ymacro-annotations")
    },
    scalacOptions ++= Seq(
      "-target:jvm-1.8",
      "-deprecation"
    )
  )

}
