package dataclass

import dataclass.TestUtil._
import shapeless.test.illTyped
import utest._

import scala.concurrent.Future

object TwoFieldsTests extends TestSuite {
  val tests = Tests {
    @data class Foo(a: Int, other: String)
    test("equals") {
      test {
        val foo = Foo(1, "a")
        val foo2 = Foo(1, "a")
        assert(foo == foo2)
      }
      test {
        val foo = Foo(1, "a")
        val foo2 = Foo(3, "a")
        val foo3 = Foo(1, "b")
        assert(foo != foo2)
        assert(foo != foo3)
        assert(foo2 != foo3)
      }
    }
    test("toString") {
      val foo = Foo(1, "o")
      val str = foo.toString
      val expected = "Foo(1, o)"
      assert(str == expected)
    }
    test("class constructor") {
      test("public") {
        // Would have preferred it to be public by default
        val foo = new Foo(3, "a")
      }
      test("private") {
        @data class Bar private (n: Int, s: String)
        val bar = Bar(4, "b")
        illTyped(
          """
          val bar0 = new Bar(5, "c")
        """,
          "constructor Bar in class Bar cannot be accessed.*"
        )
      }
    }
    test("accessors") {
      val foo = Foo(2, "c")
      assert(foo.a == 2)
      assert(foo.other == "c")
    }
    test("setters") {
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
    test("option setters") {
      @data(optionSetters = true) class Bar(
          a: Int,
          other: Option[String] = None
      )
      val bar = Bar(1)
      val bar2 = bar.withA(2)
      val bar3 = bar.withOther("t")
      val bar4 = bar2.withOther(Some("u"))
      val bar5 = bar2.withOther(None)
      assert(bar.a == 1)
      assert(bar.other == None)
      assert(bar2.a == 2)
      assert(bar2.other == None)
      assert(bar3.a == 1)
      assert(bar3.other == Some("t"))
      assert(bar4.a == 2)
      assert(bar4.other == Some("u"))
      assert(bar5.a == 2)
      assert(bar5.other == None)
    }

    test("tuple") {
      @data class Foo0(a: Int, other: String) {
        def tuple0 = tuple
      }
      val foo = Foo0(1, "a")
      val t = foo.tuple0
      assert(t == (1, "a"))
    }

    test("private field") {
      @data class Bar(private val n: Int, s: String)
      val bar = Bar(2, "a")
      illTyped(
        """
        bar.n
      """,
        "value n in class Bar cannot be accessed .* Bar.*"
      )
      assert(bar.s == "a")
    }

    test("shapeless") {
      @data class Bar(n: Int, s: String)
      import shapeless.{test => _, _}
      test {
        val gen = Generic[Bar]
        val bar: Bar = gen.from(2 :: "a" :: HNil)
        val l: Int :: String :: HNil = gen.to(Bar(3, "b"))
        assert(bar.n == 2)
        assert(bar.s == "a")
        assert(l == 3 :: "b" :: HNil)
      }
    }

    test("product") {
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

      if (productElemNameAvailable) {
        val names = foo.productElementNames.toVector
        val expectedNames = Seq("a", "other")
        assert(names == expectedNames)

        try {
          foo.productElementName(-1)
          assert(false)
        } catch {
          case _: IndexOutOfBoundsException =>
        }

        try {
          foo.productElementName(2)
          assert(false)
        } catch {
          case _: IndexOutOfBoundsException =>
        }
      }
    }

    test("productPrefix") {
      val foo = Foo(1, "c")
      val prefix = foo.productPrefix
      val expectedPrefix = "Foo"
      assert(prefix == expectedPrefix)
    }

    test("type params") {
      test("one") {
        test("used") {
          @data class Bar[T](t: T, n: Double)
          val barI = Bar[Int](2, 1.0)
          val barI2 = Bar[Int](3, 1.2)
          val barI3 = Bar[Int](2, 1.0)
          val barS = Bar[String]("ab", 2.1)
          assert(barI != barS)
          assert(barI != barI2)
          assert(barI == barI3)
        }

        test("unused") {
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

        test("setters") {
          @data class Bar[T](n: Int, s: String)
          val barI: Bar[Int] = Bar[Int](2, "a")
          val barNewN: Bar[Int] = barI.withN(3)
          assert(barNewN.n == 3)
        }

      }

      test("two") {
        @data class Bar[T, U](t: T, u: U)
        val barI = Bar[Int, Double](1, 1.0)
        val barS = Bar[String, Long]("a", 2L)
        assert(barI != barS)
      }

      test("three") {
        @data class Bar[T, U, V](u: U, v: V)
        val barI = Bar[Int, Double, Int](1.1, 2)
        val barS = Bar[String, Long, Long](1L, 3L)
        assert(barI != barS)
      }

      test("higher kind") {
        test("one") {
          @data class Bar[F[_]](f1: F[Int], f2: F[String])
          val barF = Bar[Future](
            Future.successful(2),
            Future.failed(new RuntimeException("nope"))
          )
          val barL = Bar[List](List(3), List("a", "b"))
          assert(barF != barL)
        }

        test("two") {
          @data class Bar[F[_], G[_]](f: F[Int], g: G[String])
          val barF = Bar[Future, Vector](Future.successful(2), Vector("a", "b"))
          val barL = Bar[List, Future](List(3), Future.successful("s"))
          assert(barF != barL)
        }
      }
    }

    test("multiple parameter groups") {
      test {
        @data class Bar(n: Int)(implicit s: String)
        val bar = Bar(2)("a")
        val bar0 = Bar(1)("a")
        assert(bar != bar0)
      }

      test {
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
