package dataclass

import shapeless.test.illTyped
import utest._

import scala.concurrent.Future

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
    "class constructor" - {
      "public" - {
        // Would have preferred it to be public by default
        val foo = new Foo(3, "a")
      }
      "private" - {
        @data class Bar private (n: Int, s: String)
        val bar = Bar(4, "b")
        illTyped("""
          val bar0 = new Bar(5, "c")
        """, "constructor Bar in class Bar cannot be accessed.*")
      }
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
      @data class Foo0(a: Int, other: String) {
        def tuple0 = tuple
      }
      val foo = Foo0(1, "a")
      val t = foo.tuple0
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
      @data class Bar(n: Int, s: String)
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

    "productPrefix" - {
      val foo = Foo(1, "c")
      val prefix = foo.productPrefix
      val expectedPrefix = "Foo"
      assert(prefix == expectedPrefix)
    }

    "type params" - {
      "one" - {
        "used" - {
          @data class Bar[T](t: T, n: Double)
          val barI = Bar[Int](2, 1.0)
          val barI2 = Bar[Int](3, 1.2)
          val barI3 = Bar[Int](2, 1.0)
          val barS = Bar[String]("ab", 2.1)
          assert(barI != barS)
          assert(barI != barI2)
          assert(barI == barI3)
        }

        "unused" - {
          @data class Bar[T](n: Int, s: String)
          val barI = Bar[Int](2, "a")
          val barI2 = Bar[Int](3, "b")
          val barI3 = Bar[Int](2, "a")
          val barS = Bar[String](4, "c")
          val barS2 = Bar[String](2, "a")
          assert(barI != barS)
          assert(barI != barI2)
          assert(barI == barI3)
          assert(barS != barS2)
          assert(barI == barS2)
        }
      }

      "two" - {
        @data class Bar[T, U](t: T, u: U)
        val barI = Bar[Int, Double](1, 1.0)
        val barS = Bar[String, Long]("a", 2L)
        assert(barI != barS)
      }

      "three" - {
        @data class Bar[T, U, V](u: U, v: V)
        val barI = Bar[Int, Double, Int](1.1, 2)
        val barS = Bar[String, Long, Long](1L, 3L)
        assert(barI != barS)
      }

      "higher kind" - {
        "one" - {
          @data class Bar[F[_]](f1: F[Int], f2: F[String])
          val barF = Bar[Future](
            Future.successful(2),
            Future.failed(new RuntimeException("nope"))
          )
          val barL = Bar[List](List(3), List("a", "b"))
          assert(barF != barL)
        }

        "two" - {
          @data class Bar[F[_], G[_]](f: F[Int], g: G[String])
          val barF = Bar[Future, Vector](Future.successful(2), Vector("a", "b"))
          val barL = Bar[List, Future](List(3), Future.successful("s"))
          assert(barF != barL)
        }
      }
    }

    "multiple parameter groups" - {
      * - {
        @data class Bar(n: Int)(implicit s: String)
        val bar = Bar(2)("a")
        val bar0 = Bar(1)("a")
        assert(bar != bar0)
      }

      * - {
        class TC[T]
        object TC {
          implicit val int = new TC[Int]
          implicit val string = new TC[String]
        }
        @data class Bar[T](t: T)(implicit f: TC[T])
        val bar = Bar(2)
        val bar0 = Bar("a")
        assert(bar != bar0)
        assert(bar.f == TC.int)
        assert(bar0.f == TC.string)
      }
    }
  }
}
