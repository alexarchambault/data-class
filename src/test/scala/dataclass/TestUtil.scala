package dataclass

object TestUtil {

  def productElemNameAvailable = dataclass.Macros.productElemNameAvailable

  implicit class ProductElementName211_212(private val product: Product) {
    def productElementName(n: Int): String = ???
    def productElementNames: Iterator[String] = ???
  }

}
