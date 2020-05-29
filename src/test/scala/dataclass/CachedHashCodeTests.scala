package dataclass

import utest._

object CachedHashCodeTests extends TestSuite {
  // This is extremely crude naturally, but we typically get orders of magnitude difference:
  //  for instance 26519 < 4382593
  private def timed[T](warmup: => T)(t: => T): Long = {
    warmup
    val t0 = System.nanoTime()
    t
    System.nanoTime() - t0
  }

  val tests = Tests {
    "cached hashCode should be much faster" - {
      val numbers = (0 to 1000).toVector

      @data(cachedHashCode = true) class Cached(numbers: Seq[Int])
      val cached = numbers.map(_ => Cached(numbers))
      val cachedMs = timed(cached.hashCode)(cached.hashCode)

      @data class NotCached(numbers: Seq[Int])
      val notCached = numbers.map(_ => NotCached(numbers))
      val notCachedMs = timed(notCached.hashCode)(notCached.hashCode)

      println(s"$cachedMs < $notCachedMs")
      assert(cachedMs < notCachedMs)
    }
  }
}
