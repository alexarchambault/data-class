package dataclass.plugintests

import scala.quoted.*

object Macros {
  inline def one: Int = ${ oneImpl }
  def oneImpl(using Quotes): Expr[Int] = Expr(1)
}
