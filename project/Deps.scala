import sbt._
import sbt.Keys.scalaVersion

object Deps {

  def scalaReflect =
    Def.setting {
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    }
  def shapeless = "com.chuusai" %% "shapeless" % "2.3.13"
  def utest = "com.lihaoyi" %% "utest" % "0.9.5"

}
