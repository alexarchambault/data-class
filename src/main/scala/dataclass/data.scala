package dataclass

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros

@compileTimeOnly("enable macro paradise to expand macro annotations")
class data(apply: Boolean = true, publicConstructor: Boolean = true)
    extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Macros.impl
}
