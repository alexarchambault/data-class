package dataclass

import shapeless.test.illTyped
import utest._

object MoreFieldsTests extends TestSuite {
  val tests = Tests {
    "since annotation" - {
      * - {
        illTyped(
          """
            @data class Bar(n: Int, s: String, @since("") b: Boolean)
          """,
          "Found parameter with no default value b after @since annotation"
        )
        illTyped(
          """
            @data class Bar(n: Int, s: String, @since("") b: Boolean = true, d: Double)
          """,
          "Found parameter with no default value d after @since annotation"
        )
      }
      * - {
        @data class Bar(
            n: Int,
            s: String,
            @since("") b: Boolean = true,
            d: Double = 1.0
        )

        val bar = Bar(2, "a")
        val bar2 = Bar(2, "a", false, 2.0)

        assert(bar.n == 2)
        assert(bar.s == "a")
        assert(bar.b == true)
        assert(bar.d == 1.0)

        assert(bar2.n == 2)
        assert(bar2.s == "a")
        assert(bar2.b == false)
        assert(bar2.d == 2.0)
      }
    }

    "shapeless" - {
      @data(publicConstructor = true) class Bar(n: Int, s: String, d: Double)
      @data(publicConstructor = true) class Baz(
          n: Int,
          s: String,
          @since d: Double = 1.0,
          b: Boolean = false
      )

      import shapeless._
      "simple" - {
        val gen = Generic[Bar]
        val bar: Bar = gen.from(2 :: "a" :: 1.0 :: HNil)
        val l: Int :: String :: Double :: HNil = gen.to(Bar(3, "b", 1.2))
        assert(bar.n == 2)
        assert(bar.s == "a")
        assert(bar.d == 1.0)
        assert(l == 3 :: "b" :: 1.2 :: HNil)
      }

      "with since" - {
        val gen = Generic[Baz]
        val baz: Baz = gen.from(2 :: "a" :: 1.0 :: true :: HNil)
        val l: Int :: String :: Double :: Boolean :: HNil =
          gen.to(Baz(3, "b", 1.2, true))
        assert(baz.n == 2)
        assert(baz.s == "a")
        assert(baz.d == 1.0)
        assert(baz.b == true)
        assert(l == 3 :: "b" :: 1.2 :: true :: HNil)
      }
    }

    "product" - {
      @data class Foo(
          n: Int,
          s: String,
          @since("") b: Boolean = true,
          d: Double = 1.0
      )

      val foo = Foo(2, "a", false, 1.2)
      val arity = foo.productArity
      val elements = foo.productIterator.toVector
      assert(arity == 4)
      assert(elements == Seq(2, "a", false, 1.2))

      try {
        foo.productElement(-1)
        assert(false)
      } catch {
        case _: IndexOutOfBoundsException =>
      }

      try {
        foo.productElement(4)
        assert(false)
      } catch {
        case _: IndexOutOfBoundsException =>
      }
    }

    "has toString" - {
      "def" - {
        @data class Foo(password: String) {
          override def toString: String = "Foo(****)"
        }

        val f = Foo("aa")
        val str = f.toString
        val expected = "Foo(****)"
        assert(str == expected)
      }

      "val" - {
        @data class Foo(password: String) {
          override val toString = "Foo(****)"
        }

        val f = Foo("aa")
        val str = f.toString
        val expected = "Foo(****)"
        assert(str == expected)
      }

      "lazy val" - {
        @data class Foo(password: String) {
          override lazy val toString = "Foo(****)"
        }

        val f = Foo("aa")
        val str = f.toString
        val expected = "Foo(****)"
        assert(str == expected)
      }

      "field" - {
        @data class Foo(password: String, override val toString: String)

        val f = Foo("aa", "123")
        val str = f.toString
        val expected = "123"
        assert(str == expected)
      }
    }
  }

}
