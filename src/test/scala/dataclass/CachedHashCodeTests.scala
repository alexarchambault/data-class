package dataclass

import utest._

object CachedHashCodeTests extends TestSuite {
  val tests = Tests {
    test("cached hashCode should work") {
      class HashCodeCount(value: Int) {
        var hashCodeCount = 0
        override def hashCode(): Int = {
          hashCodeCount += 1
          value
        }
      }

      @data(cachedHashCode = true) class Cached(value: HashCodeCount)
      @data class NotCached(value: HashCodeCount)

      val cached = Cached(new HashCodeCount(2))
      val notCached = NotCached(new HashCodeCount(2))

      assert(cached.value.hashCodeCount == 0)
      assert(notCached.value.hashCodeCount == 0)

      cached.hashCode
      notCached.hashCode
      assert(cached.value.hashCodeCount == 1)
      assert(notCached.value.hashCodeCount == 1)

      cached.hashCode
      notCached.hashCode
      assert(cached.value.hashCodeCount == 1)
      assert(notCached.value.hashCodeCount == 2)
    }
  }
}
