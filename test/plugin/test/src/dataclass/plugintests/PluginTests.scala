package dataclass.plugintests

import utest._

import scala.deriving.Mirror
import scala.util.hashing.MurmurHash3

object PluginTests extends TestSuite {

  private def invoke(
      obj: AnyRef,
      name: String,
      args: (Class[?], AnyRef)*
  ): AnyRef =
    obj.getClass.getMethod(name, args.map(_._1)*).invoke(obj, args.map(_._2)*)

  private def int(n: Int): (Class[?], AnyRef) = (classOf[Int], Int.box(n))
  private def bool(b: Boolean): (Class[?], AnyRef) =
    (classOf[Boolean], Boolean.box(b))
  private def str(s: String): (Class[?], AnyRef) = (classOf[String], s)

  val tests = Tests {

    test("since") {
      test("apply") {
        assert(invoke(Unrolled, "apply", int(1)) == Unrolled(1, false, "s", 2))
        assert(
          invoke(Unrolled, "apply", int(1), bool(true), str("t")) ==
            Unrolled(1, true, "t", 2)
        )
        assert(Unrolled(1) == Unrolled(1, false, "s", 2))
      }
      test("constructor") {
        val ctor1 = classOf[Unrolled].getConstructor(classOf[Int])
        assert(ctor1.newInstance(Int.box(1)) == Unrolled(1, false, "s", 2))
        val ctor3 = classOf[Unrolled]
          .getConstructor(classOf[Int], classOf[Boolean], classOf[String])
        assert(
          ctor3.newInstance(Int.box(1), Boolean.box(true), "t") ==
            Unrolled(1, true, "t", 2)
        )
        val classCtor = classOf[UnrolledClass].getConstructor(classOf[Int])
        assert(classCtor.newInstance(Int.box(3)).b == 1)
      }
      test("copy") {
        val value = Unrolled(1, true, "t", 3)
        assert(invoke(value, "copy", int(2)) == Unrolled(2, true, "t", 3))
        assert(
          invoke(value, "copy", int(2), bool(false), str("u")) ==
            Unrolled(2, false, "u", 3)
        )
        assert(value.copy(c = 4) == Unrolled(1, true, "t", 4))
      }
      test("fromProduct") {
        val mirror = summon[Mirror.ProductOf[Unrolled]]
        assert(mirror.fromProduct(Tuple1(1)) == Unrolled(1, false, "s", 2))
        assert(mirror.fromProduct((1, true, "t")) == Unrolled(1, true, "t", 2))
        assert(
          mirror.fromProduct((1, true, "t", 3)) == Unrolled(1, true, "t", 3)
        )
      }
      test("method") {
        assert(invoke(Methods, "sum", int(1)) == Int.box(4))
        assert(invoke(Methods, "sum", int(1), int(10)) == Int.box(13))
        assert(Methods.sum(1, 10, 100) == 111)
      }
      test("no forwarders without since") {
        val copies =
          classOf[NotCachedHashCode].getMethods.count(_.getName == "copy")
        assert(copies == 1)
        assert(classOf[NotCachedHashCode].getConstructors.length == 1)
      }
      test("hand-written overload") {
        assert(HandWrittenApply(1) == HandWrittenApply(1, 42))
        assert(
          invoke(HandWrittenApply, "apply", int(1)) == HandWrittenApply(1, 42)
        )
        // the hand-written apply(Int), and the synthetic apply(Int, Int)
        val applies =
          HandWrittenApply.getClass.getMethods.count(_.getName == "apply")
        assert(applies == 2)
      }
    }

    test("cachedHashCode") {
      val value = CachedHashCode(1, "a")
      val field = classOf[CachedHashCode].getDeclaredField("hashCode$cached")
      field.setAccessible(true)
      assert(field.getInt(value) == 0)
      val hashCode = value.hashCode
      assert(hashCode != 0)
      assert(field.getInt(value) == hashCode)
      assert(value.hashCode == hashCode)
      assert(CachedHashCode(1, "a").hashCode == hashCode)

      // the hash code is still the one the compiler generates
      val notCached = NotCachedHashCode(1, "a")
      assert(notCached.hashCode == MurmurHash3.caseClassHash(notCached))
      assert(hashCode == MurmurHash3.caseClassHash(value))
      assert(
        classOf[NotCachedHashCode].getDeclaredFields.forall(
          _.getName != "hashCode$cached"
        )
      )
    }

    test("settersCallApply") {
      val value = CallsApply(1, 2)
      def calls = CallsApply.calls.get()
      val before = calls

      assert(value.copy(a = 3) == CallsApply(3, 2))
      assert(calls == before + 2)

      assert(invoke(value, "copy", int(3)) == CallsApply(3, 2))
      assert(calls == before + 4)

      val mirror = summon[Mirror.ProductOf[CallsApply]]
      assert(mirror.fromProduct((4, 5)) == CallsApply(4, 5))
      assert(calls == before + 6)
      // forwarders of fromProduct create instances via apply too
      assert(mirror.fromProduct(Tuple1(4)) == CallsApply(4, 1))
      assert(calls == before + 8)
    }

    test("copy with type parameters") {
      test("implicit parameter defaults to field") {
        val shown = Shown(1)(using Describe.listDescribe)
        // no Describe[List] in scope: the implicit parameter of copy defaults to the field
        val copied: Shown[List] = shown.copy(value = 2)
        assert(copied.value == 2)
        assert(copied.describe.eq(Describe.listDescribe))
        assert(shown.copy(other = "a").describe.eq(Describe.listDescribe))
      }
      test("implicit parameter can be passed") {
        given Describe[Option] = Describe.optionDescribe
        val shown = Shown[Option](1)
        val copied = shown.copy(value = 2)(using Describe.optionDescribe)
        assert(copied.describe.eq(Describe.optionDescribe))
      }
      test("type parameter of the class") {
        val typed = Typed("a", 1)
        val copied: Typed[String] = typed.copy(n = 2)
        assert(copied == Typed("a", 2))
        assert(invoke(typed, "copy", (classOf[Object], "b")) == Typed("b", 1))
      }
    }
  }
}
