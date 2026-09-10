package dataclass

import utest._

import scala.reflect.runtime.universe._

@data(setters = false) class NoSetters(count: Int, s: String = "")

@data(setters = true) class ExplicitSetters(count: Int)

@data(setters = false, apply = false) class NoSettersNoApply(count: Int)

object NoSettersTests extends TestSuite {

  private def hasMethod[T: TypeTag](methodName: String): Boolean =
    typeOf[T].decl(TermName(methodName)) != NoSymbol

  val tests = Tests {
    test("no setters generated") {
      assert(!hasMethod[NoSetters]("withCount"))
      assert(!hasMethod[NoSetters]("withS"))
    }

    test("calling a setter does not compile") {
      val err = assertCompileError("""NoSetters(1, "a").withCount(2)""")
      assert(err.msg.contains("withCount"))
    }

    test("setters generated when set to true") {
      assert(hasMethod[ExplicitSetters]("withCount"))
    }

    test("other methods still generated") {
      val foo = NoSetters(1, "a")
      assert(foo.count == 1)
      assert(foo.s == "a")
      assert(foo.copy(count = 2) == NoSetters(2, "a"))
      assert(foo.copy(s = "b") == NoSetters(1, "b"))
      assert(foo == NoSetters(1, "a"))
      assert(foo.toString == "NoSetters(1, a)")
      assert(foo.productArity == 2)
    }

    test("no setters and no apply") {
      val foo = new NoSettersNoApply(1)
      assert(!hasMethod[NoSettersNoApply]("withCount"))
      assert(foo.copy(count = 2).count == 2)
    }
  }
}
