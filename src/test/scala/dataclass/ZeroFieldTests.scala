package dataclass

import shapeless.test.illTyped
import utest._

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
