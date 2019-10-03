package dataclass

import shapeless.test.illTyped
import utest._

import scala.concurrent.Future

object ZeroFieldTests extends TestSuite {
  val tests = Tests {
    @data class Foo()
    "equals" - {
      val foo = Foo()
      val foo2 = Foo()
      assert(foo == foo2)
    }
    "toString" - {
      val foo = Foo()
      val str = foo.toString
      val expected = "Foo()"
      assert(str == expected)
    }
    "tuple" - {
      @data class Foo0() {
        def tuple0 = tuple
      }

      * - {
        val foo = Foo0()
        val t = foo.tuple0
        assert(t == ())
      }

      "actually private" - {
        val foo = Foo0()
        illTyped("""
          foo.tuple
        """, "method tuple in class Foo0 cannot be accessed in Foo0")
      }
    }

    "type params" - {
      "one" - {
        @data class Bar[T]()
        val barI = Bar[Int]()
        val barS = Bar[String]()
        assert(barI == barS) // type erasure, both instances are the same at runtime
      }

      "two" - {
        @data class Bar[T, U]()
        val barI = Bar[Int, Double]()
        val barS = Bar[String, Long]()
        assert(barI == barS) // type erasure, both instances are the same at runtime
      }

      "three" - {
        @data class Bar[T, U, V]()
        val barI = Bar[Int, Double, Int]()
        val barS = Bar[String, Long, Long]()
        assert(barI == barS) // type erasure, both instances are the same at runtime
      }

      "higher kind" - {
        @data class Bar[F[_]]()
        val barF = Bar[Future]()
        val barL = Bar[List]()
        assert(barF == barL) // type erasure, both instances are the same at runtime
      }
    }

    "class constructor is private" - {
      illTyped("""
      val foo = new Foo()
    """, "constructor Foo in class Foo cannot be accessed .*")
    }

    "shapeless" - {
      @data(publicConstructor = true) class Bar()
      import shapeless._
      * - {
        val gen = Generic[Bar]
        val bar: Bar = gen.from(HNil)
        val l: HNil = gen.to(Bar())
      }
    }

    "product" - {
      val foo = Foo()
      val arity = foo.productArity
      val elements = foo.productIterator.toVector
      assert(arity == 0)
      assert(elements.isEmpty)

      try {
        foo.productElement(-1)
        assert(false)
      } catch {
        case _: IndexOutOfBoundsException =>
      }

      try {
        foo.productElement(0)
        assert(false)
      } catch {
        case _: IndexOutOfBoundsException =>
      }
    }

    "serializable" - {
      val foo = Foo()
      assert(foo.isInstanceOf[Serializable])
    }
  }

}
