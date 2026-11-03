package dataclass

import scala.annotation.StaticAnnotation

/** Scala 3 counterpart of the Scala 2 `@since` macro annotation.
  *
  * Marks the first parameter added in a given version: the data-class compiler
  * plugin generates the binary-compatible overloads (of the constructor,
  * `apply`, `copy`, or any final method) without that parameter and the
  * following ones, that need default values.
  */
class since(val version: String = "") extends StaticAnnotation
