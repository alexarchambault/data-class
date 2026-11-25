package dataclass.plugintests

import dataclass.{data, since}

import java.util.concurrent.atomic.AtomicInteger

@data(nullOptionBuilderMethodName = "from") case class Builder(
    n: Int = 2,
    s: String = "default",
    bar: Boolean = true,
    opt: Option[String] = None,
    optInt: Option[Int] = Some(3),
    optList: Option[List[Int]] = None
)

@data(nullOptionBuilderMethodName = "from") case class BuilderRequired(
    id: String,
    count: Int,
    name: Option[String]
)

@data(nullOptionBuilderMethodName = "from") case class BuilderEvolving(
    n: Int = 1,
    @since s: String = "a",
    opt: Option[Long] = None
)

@data(nullOptionBuilderMethodName = "from") case class BuilderGeneric[T](
    value: T,
    opt: Option[T] = None,
    list: List[T] = Nil
)

@data(nullOptionBuilderMethodName = "from") case class BuilderImplicit[F[_]](
    n: Int = 0
)(implicit val describe: Describe[F])

@data(nullOptionBuilderMethodName = "from", settersCallApply = true)
case class BuilderApply(s: String = "")
object BuilderApply {
  val calls = new AtomicInteger
  def apply(s: String): BuilderApply = {
    calls.incrementAndGet()
    new BuilderApply(s)
  }
}

@data(nullOptionBuilderMethodName = "from") case class BuilderHandWritten(
    n: Int = 0
)
object BuilderHandWritten {
  def from(n: Int): BuilderHandWritten = new BuilderHandWritten(n + 1)
}

@data case class NoBuilder(n: Int = 0)
