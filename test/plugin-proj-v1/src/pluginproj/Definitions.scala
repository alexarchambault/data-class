package pluginproj

import dataclass.{data, since}

@data case class Foo(n: Int, s: String, @since b: Boolean = false)

final class Bar(val a: Int, @since val b: Int = 1) {
  override def toString = s"Bar($a, $b)"
}

object Methods {
  def greet(name: String): String = s"Hello $name"
}
