package dataclass

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros

@compileTimeOnly("enable macro paradise to expand macro annotations")
class data(
    apply: Boolean = true,
    publicConstructor: Boolean = true,
    /** Whether to generate `withFoo(foo: Foo)` methods for fields like `foo:
      * Option[Foo]`)
      */
    optionSetters: Boolean = false,
    /** Whether setters will call apply or new */
    settersCallApply: Boolean = false,
    /** Whether hashCode will be cached */
    cachedHashCode: Boolean = false
) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Macros.impl
}
