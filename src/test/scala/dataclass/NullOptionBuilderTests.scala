package dataclass

import utest._

import scala.reflect.runtime.universe._

@data(nullOptionBuilderMethodName = "from") class Builder(
    n: Int = 2,
    s: String = "default",
    bar: Boolean = true,
    opt: Option[String] = None,
    optInt: Option[Int] = Some(3),
    optList: Option[List[Int]] = None
)

@data(nullOptionBuilderMethodName = "from") class BuilderRequired(
    id: String,
    count: Int,
    name: Option[String]
)

@data(nullOptionBuilderMethodName = "from") class BuilderEvolving(
    n: Int = 1,
    @since
    s: String = "a",
    opt: Option[Long] = None
)

@data(nullOptionBuilderMethodName = "from") class BuilderGeneric[T](
    value: T,
    opt: Option[T] = None,
    list: List[T] = Nil
)

@data(
  nullOptionBuilderMethodName = "from",
  settersCallApply = true,
  apply = false
) class BuilderApply(s: String = "")

object BuilderApply {
  var applyCalls = 0
  def apply(s: String): BuilderApply = {
    applyCalls += 1
    new BuilderApply(s)
  }
}

@data(nullOptionBuilderMethodName = "from") class BuilderHandWritten(n: Int = 0)

object BuilderHandWritten {
  def from(n: Int): BuilderHandWritten = new BuilderHandWritten(n + 1)
}

@data class NoBuilder(n: Int = 0)

@data(nullOptionBuilderMethodName = null) class NullBuilderName(n: Int = 0)

object NullOptionBuilderTests extends TestSuite {

  private def hasMethod[T: TypeTag](methodName: String): Boolean =
    typeOf[T].companion.decl(TermName(methodName)) != NoSymbol

  private val default = Builder(2, "default", true, None, Some(3), None)

  val tests = Tests {
    test("defaults") {
      assert(Builder.from() == default)
      assert(Builder.from(null, null, null, null, null, null) == default)
    }

    test("values") {
      assert(Builder.from(n = 4) == default.withN(4))
      assert(Builder.from(s = "b") == default.withS("b"))
      assert(Builder.from(bar = false) == default.withBar(false))
      assert(Builder.from(opt = "c") == default.withOpt(Some("c")))
      assert(Builder.from(optInt = 5) == default.withOptInt(Some(5)))
      assert(
        Builder.from(optList = List(1)) == default.withOptList(Some(List(1)))
      )
    }

    test("boxed types") {
      val n: java.lang.Integer = 6
      val bar: java.lang.Boolean = java.lang.Boolean.FALSE
      val optInt: java.lang.Integer = null
      assert(
        Builder.from(n = n, bar = bar, optInt = optInt) ==
          default.withN(6).withBar(false)
      )
      val err = assertCompileError("""Builder.from(opt = Some("a"))""")
      assert(err.msg.contains("type mismatch"))
    }

    test("required fields") {
      assert(BuilderRequired.from("a", 1) == BuilderRequired("a", 1, None))
      assert(
        BuilderRequired.from("a", 1, "b") == BuilderRequired("a", 1, Some("b"))
      )
      assertCompileError("""BuilderRequired.from(count = 1)""")
    }

    test("since overloads") {
      assert(BuilderEvolving.from() == BuilderEvolving(1, "a", None))
      assert(BuilderEvolving.from(2) == BuilderEvolving(2, "a", None))
      assert(BuilderEvolving.from(null, "b") == BuilderEvolving(1, "b", None))
      assert(
        BuilderEvolving.from(opt = 3L) == BuilderEvolving(1, "a", Some(3L))
      )
      // the overload for the fields before @since, kept for binary compatibility
      val n: java.lang.Integer = 2
      assert(BuilderEvolving.from(n) == BuilderEvolving(2, "a", None))
    }

    test("type parameters") {
      // fields whose type is a type parameter of the class keep it
      assert(BuilderGeneric.from(1) == BuilderGeneric(1, None, Nil))
      assert(BuilderGeneric.from(1, Some(2)) == BuilderGeneric(1, Some(2), Nil))
      assert(
        BuilderGeneric.from("a", list = List("b")) ==
          BuilderGeneric("a", None, List("b"))
      )
      assert(
        BuilderGeneric.from[String]("a", list = null) ==
          BuilderGeneric("a", None, Nil)
      )
    }

    test("settersCallApply") {
      val before = BuilderApply.applyCalls
      assert(BuilderApply.from().s == "")
      assert(BuilderApply.from("a").s == "a")
      assert(BuilderApply.applyCalls == before + 2)
    }

    test("hand-written") {
      assert(BuilderHandWritten.from(1) == BuilderHandWritten(2))
    }

    test("disabled") {
      assert(!hasMethod[NoBuilder]("from"))
      assert(!hasMethod[NullBuilderName]("from"))
      assert(hasMethod[Builder]("from"))
    }
  }
}
