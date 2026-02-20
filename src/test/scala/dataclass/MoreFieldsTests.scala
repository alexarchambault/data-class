package dataclass

import dataclass.TestUtil._
import shapeless.test.illTyped
import utest._

object MoreFieldsTests extends TestSuite {
  val tests = Tests {
    test("tuple") {
      @data class Bar(
          n: Int,
          s: String,
          b: Boolean = true,
          d: Double = 1.0
      ) {
        def tuple0 = tuple
      }
      val foo = Bar(1, "a", false, 1.2)
      val t = foo.tuple0
      assert(t == (1, "a", false, 1.2))
    }

    test("option setters") {
      @data(optionSetters = true) class Bar(
          n: Int,
          s: String,
          b: Option[Boolean] = None,
          d: Double = 1.0
      )
      val foo = Bar(1, "a")
      val foo2 = foo.withB(true)
      val foo3 = foo.withB(Some(false))
      val foo4 = foo2.withB(None)
      assert(foo.b == None)
      assert(foo2.b == Some(true))
      assert(foo3.b == Some(false))
      assert(foo4.b == None)
    }

    test("since annotation") {
      test {
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
      test {
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

    test("shapeless") {
      @data class Bar(n: Int, s: String, d: Double)
      @data class Baz(
          n: Int,
          s: String,
          @since d: Double = 1.0,
          b: Boolean = false
      )

      import shapeless.{test => _, _}
      test("simple") {
        val gen = Generic[Bar]
        val bar: Bar = gen.from(2 :: "a" :: 1.0 :: HNil)
        val l: Int :: String :: Double :: HNil = gen.to(Bar(3, "b", 1.2))
        assert(bar.n == 2)
        assert(bar.s == "a")
        assert(bar.d == 1.0)
        assert(l == 3 :: "b" :: 1.2 :: HNil)
      }

      test("with since") {
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

    test("product") {
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

      if (productElemNameAvailable) {
        val names = foo.productElementNames.toVector
        val expectedNames = Seq("n", "s", "b", "d")
        assert(names == expectedNames)

        try {
          foo.productElementName(-1)
          assert(false)
        } catch {
          case _: IndexOutOfBoundsException =>
        }

        try {
          foo.productElementName(4)
          assert(false)
        } catch {
          case _: IndexOutOfBoundsException =>
        }
      }
    }

    test("productPrefix") {
      @data class Foo(
          n: Int,
          s: String,
          @since("") b: Boolean = true,
          d: Double = 1.0
      )

      val foo = Foo(1, "a")
      val prefix = foo.productPrefix
      val expectedPrefix = "Foo"
      assert(prefix == expectedPrefix)
    }

    test("has toString") {
      test("def") {
        @data class Foo(password: String) {
          override def toString: String = "Foo(****)"
        }

        val f = Foo("aa")
        val str = f.toString
        val expected = "Foo(****)"
        assert(str == expected)
      }

      test("val") {
        @data class Foo(password: String) {
          override val toString = "Foo(****)"
        }

        val f = Foo("aa")
        val str = f.toString
        val expected = "Foo(****)"
        assert(str == expected)
      }

      test("lazy val") {
        @data class Foo(password: String) {
          override lazy val toString = "Foo(****)"
        }

        val f = Foo("aa")
        val str = f.toString
        val expected = "Foo(****)"
        assert(str == expected)
      }

      test("field") {
        @data class Foo(password: String, override val toString: String)

        val f = Foo("aa", "123")
        val str = f.toString
        val expected = "123"
        assert(str == expected)
      }
    }

    test("has hashCode") {
      test("def") {
        @data class Foo(password: String) {
          override def hashCode: Int = 34
        }

        val f = Foo("aa")
        val code = f.hashCode
        val expected = 34
        assert(code == expected)
      }

      test("val") {
        @data class Foo(password: String) {
          override val hashCode = 34
        }

        val f = Foo("aa")
        val code = f.hashCode
        val expected = 34
        assert(code == expected)
      }

      test("lazy val") {
        @data class Foo(password: String) {
          override lazy val hashCode = 34
        }

        val f = Foo("aa")
        val code = f.hashCode
        val expected = 34
        assert(code == expected)
      }

      test("hashCode") {
        @data class Foo(password: String, override val hashCode: Int)

        val f = Foo("aa", 56)
        val code = f.hashCode
        val expected = 56
        assert(code == expected)
      }
    }

    test("has equals") {
      test("def") {
        @data class Foo(password: String) {
          override def equals(obj: Any): Boolean =
            obj.isInstanceOf[Foo] && (
              password == obj.asInstanceOf[Foo].password ||
                obj.asInstanceOf[Foo].password == "special"
            )
        }

        val foo = Foo("aa")
        val other = Foo("bb")
        val specialFoo = Foo("special")
        val equalsCopy = foo == foo.withPassword("aa")
        assert(equalsCopy)
        val equalsOther = foo == other
        assert(!equalsOther)
        val equalsSpecial = foo == specialFoo
        assert(equalsSpecial)
      }

      test("two-arg") {
        @data class Foo(password: String) {
          def equals(obj: Any, thing: Any): Boolean =
            true
        }

        val foo = Foo("aa")
        val other = Foo("bb")
        val specialFoo = Foo("special")
        val equalsCopy = foo == foo.withPassword("aa")
        assert(equalsCopy)
        val equalsOther = foo == other
        assert(!equalsOther)
        val equalsSpecial = foo == specialFoo
        assert(!equalsSpecial)
      }
    }

    test("has canEqual") {
      test("def") {
        @data class Foo(password: String) {
          override def canEqual(obj: Any): Boolean =
            obj.isInstanceOf[Foo] || obj.toString == "Foo"
        }

        val foo = Foo("aa")
        val other = Foo("bb")
        val canEqualOther = foo.canEqual(other)
        assert(canEqualOther)
        val canEqualRandomString = foo.canEqual("thing")
        assert(!canEqualRandomString)
        val canEqualSpecialString = foo.canEqual("Foo")
        assert(canEqualSpecialString)
      }

      test("two-arg") {
        @data class Foo(password: String) {
          def canEqual(obj: Any, thing: Any): Boolean =
            obj.isInstanceOf[Foo] || obj.toString == "Foo"
        }

        val foo = Foo("aa")
        val other = Foo("bb")
        val canEqualOther = foo.canEqual(other)
        assert(canEqualOther)
        val canEqualRandomString = foo.canEqual("thing")
        assert(!canEqualRandomString)
        val canEqualSpecialString = foo.canEqual("Foo")
        assert(!canEqualSpecialString)
      }
    }

    test("override val with default") {
      class Repository {
        def versionsCheckHasModule: Boolean = false
      }
      @data class IvyRepository(
          override val
          versionsCheckHasModule: Boolean = true
      ) extends Repository
    }
  }

}
