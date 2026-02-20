package dataclass

import dataclass.TestUtil._
import shapeless.test.illTyped
import utest._

import scala.concurrent.Future

object OneFieldTests extends TestSuite {
  val tests = Tests {
    @data class Foo(count: Int)
    test("equals") {
      test {
        val foo = Foo(1)
        val foo2 = Foo(1)
        assert(foo == foo2)
      }
      test {
        val foo = Foo(1)
        val foo2 = Foo(3)
        assert(foo != foo2)
      }
    }
    test("reference equals") {
      class Crazy() {
        var i = -1
        override def equals(obj: Any): Boolean = { i += 1; i % 2 == 0 }
      }
      @data class FooCrazy(count: Crazy)
      val crazy = new Crazy()
      val foo = FooCrazy(crazy)
      assert(crazy == crazy)
      assert(crazy != crazy)
      assert(foo == foo)
      assert(foo == foo)
    }
    test("toString") {
      val foo = Foo(1)
      val str = foo.toString
      val expected = "Foo(1)"
      assert(str == expected)
    }
    test("class constructor") {
      test("public") {
        val foo = new Foo(3)
      }
      test("private") {
        @data class Bar private (n: Int)
        val bar = Bar(2)
        illTyped(
          """
          val bar0 = new Bar(3)
        """,
          "constructor Bar in class Bar cannot be accessed.*"
        )
      }
    }
    test("accessor") {
      val foo = Foo(2)
      assert(foo.count == 2)
    }
    test("setter") {
      val foo = Foo(1)
      val foo2 = foo.withCount(2)
      assert(foo.count == 1)
      assert(foo2.count == 2)
    }
    test("option setter") {
      @data(optionSetters = true) class Bar(count: Option[Int] = None)
      val bar = Bar()
      val bar2 = bar.withCount(2)
      assert(bar.count == None)
      assert(bar2.count == Some(2))
    }
    test("no option setter") {
      @data class Bar(count: Option[Int] = None)
      val bar = Bar()
      illTyped(
        """
        val bar2 = bar.withCount(2)
      """,
        "type mismatch;.*"
      )
      assert(bar.count == None)
    }
    test("setter calls apply") {
      @data(apply = false, settersCallApply = true) class CrazyBar(count: Int)
      object CrazyBar {
        def apply(count: Int): CrazyBar = new CrazyBar(0)
      }
      val bar = new CrazyBar(1)
      assert(bar.count == 1)
      assert(bar.withCount(2).count == 0)
    }

    test("tuple") {
      @data class Foo0(a: Int) {
        def tuple0 = tuple
      }
      val foo = Foo0(1)
      val t = foo.tuple0
      assert(t == Tuple1(1))
    }

    test("private field") {
      @data class Bar(private val n: Int)
      val bar = Bar(2)
      illTyped(
        """
        bar.n
      """,
        "value n in class Bar cannot be accessed .* Bar.*"
      )
    }

    test("default value") {
      @data class Bar(n: Int = 2)
      test {
        val bar = Bar()
        assert(bar.n == 2)
      }
      test {
        val bar = Bar(3)
        assert(bar.n == 3)
      }
      test {
        val bar = Bar(3)
        illTyped(
          """
          bar.withN()
        """,
          "not enough arguments for method withN.*"
        )
      }
    }

    test("shapeless") {
      @data class Bar(n: Int)
      import shapeless.{test => _, _}
      test {
        val gen = Generic[Bar]
        val bar: Bar = gen.from(2 :: HNil)
        val l: Int :: HNil = gen.to(Bar(3))
        assert(bar.n == 2)
        assert(l == 3 :: HNil)
      }
    }

    test("product") {
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

      if (productElemNameAvailable) {
        val names = foo.productElementNames.toVector
        val expectedNames = Seq("count")
        assert(names == expectedNames)

        try {
          foo.productElementName(-1)
          assert(false)
        } catch {
          case _: IndexOutOfBoundsException =>
        }

        try {
          foo.productElementName(1)
          assert(false)
        } catch {
          case _: IndexOutOfBoundsException =>
        }
      }
    }

    test("productPrefix") {
      val foo = Foo(1)
      val prefix = foo.productPrefix
      val expectedPrefix = "Foo"
      assert(prefix == expectedPrefix)
    }

    test("type params") {
      test("one") {
        test("used") {
          @data class Bar[T](t: T)
          val barI = Bar[Int](2)
          val barI2 = Bar[Int](3)
          val barI3 = Bar[Int](2)
          val barS = Bar[String]("ab")
          assert(barI != barS)
          assert(barI != barI2)
          assert(barI == barI3)
        }

        test("unused") {
          @data class Bar[T](n: Int)
          val barI = Bar[Int](2)
          val barI2 = Bar[Int](3)
          val barI3 = Bar[Int](2)
          val barS = Bar[String](4)
          val barS2 = Bar[String](2)
          assert(barI != barS)
          assert(barI != barI2)
          assert(barI == barI3)
          assert(barS != barS2)
          assert(barI == barS2)
        }
      }

      test("two") {
        @data class Bar[T, U](u: U)
        val barI = Bar[Int, Double](1.0)
        val barS = Bar[String, Long](2L)
        assert(barI != barS)
      }

      test("three") {
        @data class Bar[T, U, V](v: V)
        val barI = Bar[Int, Double, Int](2)
        val barS = Bar[String, Long, Long](3L)
        assert(barI != barS)
      }

      test("higher kind") {
        test {
          @data class Bar[F[_]](f: F[Int])
          val barF = Bar[Future](Future.successful(2))
          val barL = Bar[List](List(3))
          assert(barF != barL)
        }

        test {
          @data class Bar[F[_, _]](f: F[String, Int])
          val bar = Bar[Map](Map("a" -> 1))
          val bar0 = Bar[Map](Map("b" -> 2))
          assert(bar != bar0)
        }

        test {
          @data class Bar[F[_[_]]](f: F[List])
          class Monad[F[_]]
          val bar = Bar[Monad](new Monad[List])
          val bar0 = Bar[Monad](new Monad[List])
          assert(bar != bar0)
        }

        test {
          @data class Bar[F[_[_, _]]](f: F[Map])
          class TC[F[_, _]]
          val bar = Bar[TC](new TC[Map])
          val bar0 = Bar[TC](new TC[Map])
          assert(bar != bar0)
        }
      }

      test("no apply methods and private constructor") {
        @data(apply = false) class Bar private (path: String)
        object Bar {
          def apply(path: String): Bar =
            new Bar(path.stripSuffix("/"))
        }

        illTyped(
          """
          val bar1 = new Bar("a")
        """,
          "constructor Bar in class Bar cannot be accessed.*"
        )

        val bar = Bar("foo/1")
        val bar0 = Bar("foo/1/")
        assert(bar == bar0)
      }
    }
  }
}
