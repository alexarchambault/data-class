package dataclass

import scala.annotation.StaticAnnotation

/** No-op compatibility annotation for sources shared with Scala 2.
  *
  * Accepts the parameters of the Scala 2 macro annotation, so that shared
  * sources can pass them, and ignores them.
  */
class data(
    apply: Boolean = true,
    publicConstructor: Boolean = true,
    optionSetters: Boolean = false,
    settersCallApply: Boolean = false,
    cachedHashCode: Boolean = false
) extends StaticAnnotation
