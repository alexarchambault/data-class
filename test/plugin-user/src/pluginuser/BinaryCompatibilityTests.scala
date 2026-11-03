package pluginuser

import pluginproj._
import utest._

import scala.deriving.Mirror

// Compiled against pluginProjV1, run against pluginProjV2
object BinaryCompatibilityTests extends TestSuite {
  val tests = Tests {
    test("apply") {
      assert(Foo(2, "a").toString == "Foo(2,a,false,true)")
      assert(Foo(2, "a", true).toString == "Foo(2,a,true,true)")
    }
    test("constructor") {
      assert(new Foo(2, "a", true).toString == "Foo(2,a,true,true)")
      assert(new Bar(1).toString == "Bar(1, 1, 2)")
      assert(new Bar(1, 3).toString == "Bar(1, 3, 2)")
    }
    test("copy") {
      val foo = Foo(2, "a")
      assert(foo.copy(n = 3).toString == "Foo(3,a,false,true)")
      assert(foo.copy(3, "b", true).toString == "Foo(3,b,true,true)")
    }
    test("fromProduct") {
      val mirror = summon[Mirror.ProductOf[Foo]]
      assert(
        mirror.fromProduct((2, "a", true)).toString == "Foo(2,a,true,true)"
      )
    }
    test("pattern matching") {
      val Foo(n, s, b) = Foo(2, "a", true)
      assert(n == 2)
      assert(s == "a")
      assert(b)
    }
    test("method") {
      assert(Methods.greet("you") == "Hello you!")
    }
  }
}
