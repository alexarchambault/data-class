package dataclass.plugintests

import dataclass.{data, since}

import java.util.concurrent.atomic.AtomicInteger

@data case class Unrolled(
    a: Int,
    @since b: Boolean = false,
    s: String = "s",
    @since c: Int = 2
)

final class UnrolledClass(val a: Int, @since val b: Int = 1) {
  override def toString = s"UnrolledClass($a, $b)"
}

object Methods {
  def sum(a: Int, @since b: Int = 1, @since c: Int = 2): Int = a + b + c
}

// The hand-written apply takes precedence over the forwarder the plugin would generate
@data case class HandWrittenApply(a: Int, @since b: Int = 1)
object HandWrittenApply {
  def apply(a: Int): HandWrittenApply = new HandWrittenApply(a, 42)
}

@data(cachedHashCode = true) case class CachedHashCode(a: Int, s: String)

@data case class NotCachedHashCode(a: Int, s: String)

@data(settersCallApply = true) case class CallsApply(a: Int, @since b: Int = 1)
object CallsApply {
  val calls = new AtomicInteger
  def apply(a: Int, b: Int): CallsApply = {
    calls.incrementAndGet()
    new CallsApply(a, b)
  }
}

trait Describe[F[_]] {
  def name: String
}

object Describe {
  // not given / implicit, so that they're not found unless imported
  val listDescribe: Describe[List] = new Describe[List] { def name = "list" }
  val optionDescribe: Describe[Option] = new Describe[Option] { def name = "option" }
}

@data case class Shown[F[_]](value: Int, @since other: String = "")(implicit
    val describe: Describe[F]
)

@data case class Typed[A](a: A, @since n: Int = 0)
