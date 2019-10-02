package dataclass

import shapeless.test.illTyped
import utest._

object TwoFieldsTests extends TestSuite {
  val tests = Tests {
    @data class Foo(a: Int, other: String)
    "equals" - {
      * - {
        val foo = Foo(1, "a")
        val foo2 = Foo(1, "a")
        assert(foo == foo2)
      }
      * - {
        val foo = Foo(1, "a")
        val foo2 = Foo(3, "a")
        val foo3 = Foo(1, "b")
        assert(foo != foo2)
        assert(foo != foo3)
        assert(foo2 != foo3)
      }
    }
    "toString" - {
      val foo = Foo(1, "o")
      val str = foo.toString
      val expected = "Foo(1, o)"
      assert(str == expected)
    }
    "class constructor is private" - {
      illTyped("""
        val foo = new Foo(3, "a")
      """, "constructor Foo in class Foo cannot be accessed .*")
    }
    "accessors" - {
      val foo = Foo(2, "c")
      assert(foo.a == 2)
      assert(foo.other == "c")
    }
    "setters" - {
      val foo = Foo(1, "s")
      val foo2 = foo.withA(2)
      val foo3 = foo.withOther("t")
      val foo4 = foo2.withOther("u")
      assert(foo.a == 1)
      assert(foo.other == "s")
      assert(foo2.a == 2)
      assert(foo2.other == "s")
      assert(foo3.a == 1)
      assert(foo3.other == "t")
      assert(foo4.a == 2)
      assert(foo4.other == "u")
    }

    "tuple" - {
      val foo = Foo(1, "a")
      val t = foo.tuple
      assert(t == (1, "a"))
    }

    "private field" - {
      @data class Bar(private val n: Int, s: String)
      val bar = Bar(2, "a")
      illTyped("""
        bar.n
      """, "value n in class Bar cannot be accessed in Bar")
      assert(bar.s == "a")
    }

    "shapeless" - {
      @data(publicConstructor = true) class Bar(n: Int, s: String)
      import shapeless._
      * - {
        val gen = Generic[Bar]
        val bar: Bar = gen.from(2 :: "a" :: HNil)
        val l: Int :: String :: HNil = gen.to(Bar(3, "b"))
        assert(bar.n == 2)
        assert(bar.s == "a")
        assert(l == 3 :: "b" :: HNil)
      }
    }

    "product" - {
      val foo = Foo(2, "a")
      val arity = foo.productArity
      val elements = foo.productIterator.toVector
      assert(arity == 2)
      assert(elements == Seq(2, "a"))

      try {
        foo.productElement(-1)
        assert(false)
      } catch {
        case _: IndexOutOfBoundsException =>
      }

      try {
        foo.productElement(2)
        assert(false)
      } catch {
        case _: IndexOutOfBoundsException =>
      }
    }
  }
}
