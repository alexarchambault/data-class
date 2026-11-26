package dataclass.plugintests

import utest._

import scala.compiletime.testing.typeCheckErrors

object BuilderTests extends TestSuite {

  private def hasMethod(obj: AnyRef, name: String): Boolean =
    obj.getClass.getMethods.exists(_.getName == name)

  // Outside of Tests, whose macro fails to re-type the implicit conversion to java.lang.Long
  // along with the named argument on Scala 3.3
  private def evolvingWithOpt = BuilderEvolving.from(opt = 3L)

  val tests = Tests {
    test("defaults") {
      assert(Builder.from() == Builder())
      assert(Builder.from(null, null, null, null, null, null) == Builder())
    }

    test("values") {
      assert(Builder.from(n = 4) == Builder(n = 4))
      assert(Builder.from(s = "b") == Builder(s = "b"))
      assert(Builder.from(bar = false) == Builder(bar = false))
      assert(Builder.from(opt = "c") == Builder(opt = Some("c")))
      assert(Builder.from(optInt = 5) == Builder(optInt = Some(5)))
      assert(
        Builder.from(optList = List(1)) == Builder(optList = Some(List(1)))
      )
    }

    test("boxed types") {
      val n: java.lang.Integer = 6
      val bar: java.lang.Boolean = java.lang.Boolean.FALSE
      val optInt: java.lang.Integer = null
      assert(
        Builder.from(n = n, bar = bar, optInt = optInt) ==
          Builder(n = 6, bar = false)
      )
      val method = Builder.getClass.getMethods.find(_.getName == "from").get
      assert(
        method.getParameterTypes.toList == List(
          classOf[java.lang.Integer],
          classOf[String],
          classOf[java.lang.Boolean],
          classOf[String],
          classOf[java.lang.Integer],
          classOf[List[?]]
        )
      )
      assert(typeCheckErrors("""Builder.from(opt = Some("a"))""").nonEmpty)
    }

    test("required fields") {
      assert(BuilderRequired.from("a", 1) == BuilderRequired("a", 1, None))
      assert(
        BuilderRequired.from("a", 1, "b") == BuilderRequired("a", 1, Some("b"))
      )
      assert(typeCheckErrors("""BuilderRequired.from(count = 1)""").nonEmpty)
    }

    test("since overloads") {
      assert(BuilderEvolving.from() == BuilderEvolving())
      assert(BuilderEvolving.from(2) == BuilderEvolving(2))
      assert(BuilderEvolving.from(null, "b") == BuilderEvolving(1, "b"))
      assert(evolvingWithOpt == BuilderEvolving(opt = Some(3L)))
      // the overload for the fields before @since, kept for binary compatibility
      val method = BuilderEvolving.getClass.getMethod(
        "from",
        classOf[java.lang.Integer]
      )
      assert(
        method.invoke(BuilderEvolving, Int.box(2)) == BuilderEvolving(2)
      )
    }

    test("type parameters") {
      // fields whose type is a type parameter of the class keep it
      assert(BuilderGeneric.from(1) == BuilderGeneric(1))
      assert(BuilderGeneric.from(1, Some(2)) == BuilderGeneric(1, Some(2)))
      assert(
        BuilderGeneric.from("a", list = List("b")) ==
          BuilderGeneric("a", list = List("b"))
      )
      assert(
        BuilderGeneric.from[String]("a", list = null) == BuilderGeneric("a")
      )
    }

    test("implicit clause") {
      implicit val describe: Describe[List] = Describe.listDescribe
      val built = BuilderImplicit.from[List](n = 2)
      assert(built == BuilderImplicit[List](2))
      assert(built.describe.name == "list")
      assert(BuilderImplicit.from[List]().n == 0)
    }

    test("settersCallApply") {
      val before = BuilderApply.calls.get()
      assert(BuilderApply.from().s == "")
      assert(BuilderApply.from("a").s == "a")
      assert(BuilderApply.calls.get() == before + 2)
    }

    test("hand-written") {
      assert(BuilderHandWritten.from(1) == BuilderHandWritten(2))
    }

    test("disabled") {
      assert(!hasMethod(NoBuilder, "from"))
      assert(hasMethod(Builder, "from"))
    }
  }
}
