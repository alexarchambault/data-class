package dataclass.plugintests

import dataclass.{data, since}

import java.util.concurrent.atomic.AtomicInteger

@data(nullOptionCopyMethodName = "update") case class Updated(
    n: Int,
    s: String,
    bar: Boolean = true,
    opt: Option[String] = None,
    optInt: Option[Int] = None
)

@data(nullOptionCopyMethodName = "update") case class UpdatedEvolving(
    n: Int,
    @since s: String = "a",
    opt: Option[Long] = None
)

@data(nullOptionCopyMethodName = "update") case class UpdatedGeneric[T](
    value: T,
    opt: Option[T],
    list: List[T]
)

@data(nullOptionCopyMethodName = "update") case class UpdatedImplicit[F[_]](
    n: Int
)(implicit val describe: Describe[F])

@data(nullOptionCopyMethodName = "update", settersCallApply = true)
case class UpdatedApply(s: String)
object UpdatedApply {
  val calls = new AtomicInteger
  def apply(s: String): UpdatedApply = {
    calls.incrementAndGet()
    new UpdatedApply(s)
  }
}

@data(nullOptionCopyMethodName = "update") case class UpdatedHandWritten(
    n: Int
) {
  def update(n: Int): UpdatedHandWritten = new UpdatedHandWritten(n + 1)
}

@data(
  nullOptionBuilderMethodName = "from",
  nullOptionCopyMethodName = "update"
) case class UpdatedAndBuilt(n: Int = 0, opt: Option[String] = None)

@data case class NotUpdated(n: Int)
