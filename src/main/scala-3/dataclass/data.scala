package dataclass

import scala.annotation.StaticAnnotation

/** Scala 3 counterpart of the Scala 2 `@data` macro annotation, for case
  * classes in sources shared with Scala 2.
  *
  * Accepts the parameters of the Scala 2 macro annotation, so that shared
  * sources can pass them. The data-class compiler plugin implements
  * `cachedHashCode` and `settersCallApply`. `apply = false` needs nothing: a
  * companion `apply` with the constructor's signature already replaces the
  * synthetic one. The other parameters only concern the setters the Scala 2
  * macro generates, and are ignored, as no setter is generated on Scala 3.
  */
class data(
    apply: Boolean = true,
    publicConstructor: Boolean = true,
    optionSetters: Boolean = false,
    settersCallApply: Boolean = false,
    cachedHashCode: Boolean = false,
    deprecatedSetters: Boolean = false,
    deprecatedSettersMessage: String = "",
    deprecatedSettersSince: String = "",
    setters: Boolean = true
) extends StaticAnnotation
