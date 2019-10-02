package dataclass

import shapeless.test.illTyped
import utest._

object OneFieldTests extends TestSuite {
  val tests = Tests {
    @data class Foo(a: Int)
    "equals" - {
      * - {
        val foo = Foo(1)
        val foo2 = Foo(1)
        assert(foo == foo2)
      }
      * - {
        val foo = Foo(1)
        val foo2 = Foo(3)
        assert(foo != foo2)
      }
    }
    "toString" - {
      val foo = Foo(1)
      val str = foo.toString
      val expected = "Foo(1)"
      assert(str == expected)
    }
    "class constructor is private" - {
      illTyped("""
        val foo = new Foo(3)
      """, "constructor Foo in class Foo cannot be accessed .*")
    }
    "accessor" - {
      val foo = Foo(2)
      assert(foo.a == 2)
    }
    "setter" - {
      val foo = Foo(1)
      val foo2 = foo.withA(2)
      assert(foo.a == 1)
      assert(foo2.a == 2)
    }

    "tuple" - {
      val foo = Foo(1)
      val t = foo.tuple
      assert(t == Tuple1(1))
    }

    "private field" - {
      @data class Bar(private val n: Int)
      val bar = Bar(2)
      illTyped("""
        bar.n
      """, "value n in class Bar cannot be accessed in Bar")
    }

    "default value" - {
      @data class Bar(n: Int = 2)
      * - {
        val bar = Bar()
        assert(bar.n == 2)
      }
      * - {
        val bar = Bar(3)
        assert(bar.n == 3)
      }
      * - {
        val bar = Bar(3)
        illTyped("""
          bar.withN()
        """, "not enough arguments for method withN.*")
      }
    }

    "shapeless" - {
      @data(publicConstructor = true) class Bar(n: Int)
      import shapeless._
      * - {
        val gen = Generic[Bar]
        val bar: Bar = gen.from(2 :: HNil)
        val l: Int :: HNil = gen.to(Bar(3))
        assert(bar.n == 2)
        assert(l == 3 :: HNil)
      }
    }

    "product" - {
      val foo = Foo(2)
      val arity = foo.productArity
      val elements = foo.productIterator.toVector
      assert(arity == 1)
      assert(elements == Seq(2))

      try {
        foo.productElement(-1)
        assert(false)
      } catch {
        case _: IndexOutOfBoundsException =>
      }

      try {
        foo.productElement(1)
        assert(false)
      } catch {
        case _: IndexOutOfBoundsException =>
      }
    }
  }
}
