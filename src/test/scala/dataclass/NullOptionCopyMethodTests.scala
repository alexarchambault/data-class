package dataclass

import utest._

@data(nullOptionCopyMethodName = "update") class Updated(
    n: Int,
    s: String,
    bar: Boolean = true,
    opt: Option[String] = None,
    optInt: Option[Int] = None
)

@data(nullOptionCopyMethodName = "update") class UpdatedEvolving(
    n: Int,
    @since
    s: String = "a",
    opt: Option[Long] = None
)

@data(nullOptionCopyMethodName = "update") class UpdatedGeneric[T](
    value: T,
    opt: Option[T],
    list: List[T]
)

class Show[T](val name: String)

@data(nullOptionCopyMethodName = "update") class UpdatedImplicit[T](
    n: Int
)(implicit val show: Show[T])

@data(
  nullOptionCopyMethodName = "update",
  settersCallApply = true,
  apply = false
) class UpdatedApply(s: String)

object UpdatedApply {
  var applyCalls = 0
  def apply(s: String): UpdatedApply = {
    applyCalls += 1
    new UpdatedApply(s)
  }
}

@data(nullOptionCopyMethodName = "update") class UpdatedHandWritten(n: Int) {
  def update(n: Int): UpdatedHandWritten = new UpdatedHandWritten(n + 1)
}

@data(
  nullOptionBuilderMethodName = "from",
  nullOptionCopyMethodName = "update"
) class UpdatedAndBuilt(n: Int = 0, opt: Option[String] = None)

object NullOptionCopyMethodTests extends TestSuite {

  private val foo = Updated(1, "a", false, Some("b"), Some(2))

  val tests = Tests {
    test("null keeps the current value") {
      assert(foo.update() == foo)
      assert(foo.update(null, null, null, null, null) == foo)
      val empty = Updated(1, "a", true, None, None)
      assert(empty.update() == empty)
    }

    test("values") {
      assert(foo.update(n = 3) == foo.withN(3))
      assert(foo.update(s = "c") == foo.withS("c"))
      assert(foo.update(bar = true) == foo.withBar(true))
      assert(foo.update(opt = "d") == foo.withOpt(Some("d")))
      assert(foo.update(optInt = 4) == foo.withOptInt(Some(4)))
    }

    test("boxed types") {
      val n: java.lang.Integer = 5
      val optInt: java.lang.Integer = null
      assert(foo.update(n = n, optInt = optInt) == foo.withN(5))
      val err = assertCompileError("""foo.update(opt = Some("a"))""")
      assert(err.msg.contains("type mismatch"))
    }

    test("since overloads") {
      val evolving = UpdatedEvolving(1, "b", Some(2L))
      assert(evolving.update() == evolving)
      assert(evolving.update(opt = 3L) == UpdatedEvolving(1, "b", Some(3L)))
      // the overload for the fields before @since, kept for binary compatibility
      val n: java.lang.Integer = 2
      assert(evolving.update(n) == UpdatedEvolving(2, "b", Some(2L)))
      assert(evolving.update(null: java.lang.Integer) == evolving)
    }

    test("type parameters") {
      // fields whose type is a type parameter of the class keep it
      val generic = UpdatedGeneric(1, Some(2), List(3))
      assert(generic.update() == generic)
      assert(generic.update(value = 4) == generic.withValue(4))
      assert(generic.update(opt = None) == generic.withOpt(None))
      assert(generic.update(list = null) == generic)
      assert(generic.update(list = List(5)) == generic.withList(List(5)))
    }

    test("implicit parameters") {
      implicit val show: Show[Int] = new Show[Int]("int")
      val updatedImplicit = UpdatedImplicit[Int](1)
      val other = new Show[Int]("other")
      assert(updatedImplicit.update(n = 2).n == 2)
      assert(updatedImplicit.update(n = 2).show eq show)
      assert(updatedImplicit.update()(other).show eq other)
    }

    test("settersCallApply") {
      val before = UpdatedApply.applyCalls
      val updatedApply = new UpdatedApply("a")
      assert(updatedApply.update().s == "a")
      assert(updatedApply.update("b").s == "b")
      assert(UpdatedApply.applyCalls == before + 2)
    }

    test("hand-written") {
      assert(UpdatedHandWritten(1).update(1) == UpdatedHandWritten(2))
    }

    test("along with the builder") {
      val built = UpdatedAndBuilt.from(opt = "a")
      assert(built.update(n = 1) == UpdatedAndBuilt(1, Some("a")))
    }
  }
}
