import sbt._
import sbt.Keys._

object Settings {

  def scala213 = "2.13.8"
  def scala212 = "2.12.16"
  def scala211 = "2.11.12"

  private lazy val isScala211Or212 = Def.setting {
    val sv = scalaVersion.value
    sv.startsWith("2.11.") || sv.startsWith("2.12.")
  }

  lazy val shared = Def.settings(
    scalaVersion := scala213,
    crossScalaVersions := Seq(scala213, scala212, scala211),
    libraryDependencies ++= {
      if (isScala211Or212.value)
        Seq(
          compilerPlugin(
            ("org.scalamacros" % "paradise" % "2.1.1").cross(CrossVersion.full)
          )
        )
      else
        Nil
    },
    scalacOptions ++= {
      if (isScala211Or212.value) Nil
      else List("-Ymacro-annotations")
    },
    scalacOptions ++= Seq(
      "-target:jvm-1.8",
      "-deprecation"
    )
  )

}
