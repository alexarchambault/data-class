package pluginproj

import dataclass.{data, since}

@data case class Foo(
    n: Int,
    s: String,
    @since b: Boolean = false,
    @since("2.0") b1: Boolean = true
)

final class Bar(
    val a: Int,
    @since val b: Int = 1,
    @since("2.0") val c: Int = 2
) {
  override def toString = s"Bar($a, $b, $c)"
}

object Methods {
  def greet(name: String, @since("2.0") punctuation: String = "!"): String =
    s"Hello $name$punctuation"
}

@data(nullOptionBuilderMethodName = "from") case class Built(
    n: Int = 0,
    s: Option[String] = None,
    @since("2.0") b: Boolean = true,
    opt: Option[Int] = None
)
