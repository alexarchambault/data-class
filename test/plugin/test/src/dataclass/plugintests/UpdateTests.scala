package dataclass.plugintests

import utest._

import scala.compiletime.testing.typeCheckErrors

object UpdateTests extends TestSuite {

  private val foo = Updated(1, "a", false, Some("b"), Some(2))

  // Outside of Tests, whose macro fails to re-type the implicit conversion to java.lang.Long
  // along with the named argument on Scala 3.3
  private def evolvingWithOpt =
    UpdatedEvolving(1, "b", Some(2L)).update(opt = 3L)

  val tests = Tests {
    test("null keeps the current value") {
      assert(foo.update() == foo)
      assert(foo.update(null, null, null, null, null) == foo)
      val empty = Updated(1, "a")
      assert(empty.update() == empty)
    }

    test("values") {
      assert(foo.update(n = 3) == foo.copy(n = 3))
      assert(foo.update(s = "c") == foo.copy(s = "c"))
      assert(foo.update(bar = true) == foo.copy(bar = true))
      assert(foo.update(opt = "d") == foo.copy(opt = Some("d")))
      assert(foo.update(optInt = 4) == foo.copy(optInt = Some(4)))
    }

    test("boxed types") {
      val n: java.lang.Integer = 5
      val optInt: java.lang.Integer = null
      assert(foo.update(n = n, optInt = optInt) == foo.copy(n = 5))
      val method = classOf[Updated].getMethods.find(_.getName == "update").get
      assert(
        method.getParameterTypes.toList == List(
          classOf[java.lang.Integer],
          classOf[String],
          classOf[java.lang.Boolean],
          classOf[String],
          classOf[java.lang.Integer]
        )
      )
      assert(typeCheckErrors("""foo.update(opt = Some("a"))""").nonEmpty)
    }

    test("since overloads") {
      val evolving = UpdatedEvolving(1, "b", Some(2L))
      assert(evolving.update() == evolving)
      assert(evolvingWithOpt == UpdatedEvolving(1, "b", Some(3L)))
      // the overload for the fields before @since, kept for binary compatibility
      val method = classOf[UpdatedEvolving].getMethod(
        "update",
        classOf[java.lang.Integer]
      )
      assert(method.invoke(evolving, Int.box(2)) == evolving.copy(n = 2))
      assert(method.invoke(evolving, null) == evolving)
    }

    test("type parameters") {
      // fields whose type is a type parameter of the class keep it
      val generic = UpdatedGeneric(1, Some(2), List(3))
      assert(generic.update() == generic)
      assert(generic.update(value = 4) == generic.copy(value = 4))
      assert(generic.update(opt = None) == generic.copy(opt = None))
      assert(generic.update(list = null) == generic)
      assert(generic.update(list = List(5)) == generic.copy(list = List(5)))
    }

    test("implicit parameters") {
      val updatedImplicit =
        UpdatedImplicit[List](1)(using Describe.listDescribe)
      // no Describe in scope: the implicit parameter defaults to the field
      assert(updatedImplicit.update(n = 2).n == 2)
      assert(updatedImplicit.update(n = 2).describe eq Describe.listDescribe)
      val other = new Describe[List] { def name = "other" }
      assert(updatedImplicit.update()(using other).describe eq other)
    }

    test("settersCallApply") {
      val before = UpdatedApply.calls.get()
      val updatedApply = new UpdatedApply("a")
      assert(updatedApply.update().s == "a")
      assert(updatedApply.update("b").s == "b")
      assert(UpdatedApply.calls.get() == before + 2)
    }

    test("hand-written") {
      assert(UpdatedHandWritten(1).update(1) == UpdatedHandWritten(2))
    }

    test("along with the builder") {
      val built = UpdatedAndBuilt.from(opt = "a")
      assert(built.update(n = 1) == UpdatedAndBuilt(1, Some("a")))
    }

    test("disabled") {
      assert(!classOf[NotUpdated].getMethods.exists(_.getName == "update"))
    }
  }
}
