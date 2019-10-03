package dataclass

import shapeless.test.illTyped
import utest._

import scala.concurrent.Future

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
      @data class Foo0(a: Int) {
        def tuple0 = tuple
      }
      val foo = Foo0(1)
      val t = foo.tuple0
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

    "type params" - {
      "one" - {
        "used" - {
          @data class Bar[T](t: T)
          val barI = Bar[Int](2)
          val barI2 = Bar[Int](3)
          val barI3 = Bar[Int](2)
          val barS = Bar[String]("ab")
          assert(barI != barS)
          assert(barI != barI2)
          assert(barI == barI3)
        }

        "unused" - {
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

      "two" - {
        @data class Bar[T, U](u: U)
        val barI = Bar[Int, Double](1.0)
        val barS = Bar[String, Long](2L)
        assert(barI != barS)
      }

      "three" - {
        @data class Bar[T, U, V](v: V)
        val barI = Bar[Int, Double, Int](2)
        val barS = Bar[String, Long, Long](3L)
        assert(barI != barS)
      }

      "higher kind" - {
        @data class Bar[F[_]](f: F[Int])
        val barF = Bar[Future](Future.successful(2))
        val barL = Bar[List](List(3))
        assert(barF != barL)
      }
    }
  }
}
