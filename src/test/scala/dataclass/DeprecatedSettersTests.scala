package dataclass

import utest._

import scala.reflect.runtime.universe._

@data class NonDeprecatedSetters(count: Int)

@data(deprecatedSetters = true) class DeprecatedSetters(count: Int, s: String)

@data(
  deprecatedSetters = true,
  deprecatedSettersMessage = "Use copy instead",
  deprecatedSettersSince = "1.2"
) class CustomDeprecatedSetters(count: Int)

@data(deprecatedSetters = true, optionSetters = true)
class DeprecatedOptionSetters(count: Option[Int] = None)

@data(deprecatedSetters = true, settersCallApply = true)
class DeprecatedSettersCallingApply(count: Int)

object DeprecatedSettersTests extends TestSuite {

  /** `@deprecated` annotations of each `methodName` overload of `T` */
  private def deprecations[T: TypeTag](
      methodName: String
  ): Seq[Option[(String, String)]] = {
    val sym = typeOf[T].decl(TermName(methodName))
    assert(sym != NoSymbol)
    sym.alternatives.map { alt =>
      alt.annotations
        .find(_.tree.tpe.typeSymbol.fullName == "scala.deprecated")
        .map { annot =>
          // message and since, whether the reflection API exposes them as
          // plain or named arguments
          val args = annot.tree.collect {
            case Literal(Constant(str: String)) => str
          }
          assert(args.length == 2)
          (args(0), args(1))
        }
    }
  }

  val tests = Tests {
    test("not deprecated by default") {
      val found = deprecations[NonDeprecatedSetters]("withCount")
      assert(found == Seq(None))
    }

    test("deprecated setters") {
      val expected = Seq(Some(("", "")))
      assert(deprecations[DeprecatedSetters]("withCount") == expected)
      assert(deprecations[DeprecatedSetters]("withS") == expected)
    }

    test("custom deprecation parameters") {
      val found = deprecations[CustomDeprecatedSetters]("withCount")
      assert(found == Seq(Some(("Use copy instead", "1.2"))))
    }

    test("deprecated option setters") {
      // both withCount(Option[Int]) and withCount(Int) are deprecated
      val found = deprecations[DeprecatedOptionSetters]("withCount")
      assert(found.length == 2)
      assert(found.forall(_ == Some(("", ""))))
    }

    test("copy not deprecated") {
      val found = deprecations[DeprecatedSetters]("copy")
      assert(found.forall(_.isEmpty))
    }

    test("setters still work") {
      test("simple") {
        val foo = DeprecatedSetters(1, "a")
        val foo0 = foo.withCount(2)
        assert(foo0.count == 2)
        assert(foo0.s == "a")
      }
      test("option") {
        val bar = DeprecatedOptionSetters()
        val bar0 = bar.withCount(2)
        assert(bar0.count == Some(2))
      }
      test("settersCallApply") {
        val baz = DeprecatedSettersCallingApply(1)
        val baz0 = baz.withCount(2)
        assert(baz0.count == 2)
      }
    }
  }
}
