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
    /** Whether setters and copy methods will call apply or new */
    settersCallApply: Boolean = false,
    /** Whether hashCode will be cached */
    cachedHashCode: Boolean = false,
    /** Whether the generated `withFoo(foo: Foo)` setters are deprecated */
    deprecatedSetters: Boolean = false,
    /** `message` parameter of the `@deprecated` annotation added on setters
      * when `deprecatedSetters` is set
      */
    deprecatedSettersMessage: String = "",
    /** `since` parameter of the `@deprecated` annotation added on setters when
      * `deprecatedSetters` is set
      */
    deprecatedSettersSince: String = "",
    /** Whether to generate `withFoo(foo: Foo)` setters at all */
    setters: Boolean = true,
    /** Name of a method to generate in the companion, creating instances from
      * parameters accepting `null`, `null` standing for the default value of
      * the field (`None` for fields like `foo: Option[Foo]`, whose parameter is
      * `foo: Foo`). Primitive types are boxed, like `n: java.lang.Integer` for
      * a field `n: Int`. No such method is generated if `null`.
      */
    nullOptionBuilderMethodName: String = null
) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Macros.impl
}
