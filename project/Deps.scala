import sbt._
import sbt.Keys.scalaVersion

object Deps {

  def scalaReflect =
    Def.setting {
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    }
  def shapeless = "com.chuusai" %% "shapeless" % "2.3.10"
  def utest =
    Def.setting {
      val sv = scalaVersion.value
      val ver =
        if (sv.startsWith("2.11.")) "0.7.10"
        else "0.7.1"
      "com.lihaoyi" %% "utest" % ver
    }

}
