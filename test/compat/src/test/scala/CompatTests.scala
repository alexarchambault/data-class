import dataclass.{data, since => unroll}
import utest._

@data case class Foo(a: Int, @unroll b: Boolean = false)

@data case class Evolving(
    a: Int,
    @unroll b: Boolean = false,
    @unroll c: String = ""
)

object CompatTests extends TestSuite {
  val tests = Tests {
    test("construction") {
      assert(Foo(1) == Foo(1, false))
      assert(Evolving(1) == Evolving(1, false, ""))
      assert(Evolving(1, true) == Evolving(1, true, ""))
    }

    test("copy") {
      val foo = Foo(1, true)
      assert(foo.copy() == foo)
      assert(foo.copy(a = 2) == Foo(2, true))
      assert(foo.copy(3) == Foo(3, true))

      val evolving = Evolving(1, true, "value")
      assert(evolving.copy(2) == Evolving(2, true, "value"))
      assert(evolving.copy(2, false) == Evolving(2, false, "value"))
      assert(evolving.copy(c = "updated") == Evolving(1, true, "updated"))
    }
  }
}
