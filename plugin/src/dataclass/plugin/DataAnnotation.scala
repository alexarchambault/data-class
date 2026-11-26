// SPDX-License-Identifier: MIT

// Local addition, not part of upstream com-lihaoyi/unroll.

package dataclass.plugin

import dotty.tools.dotc.*
import core.*
import Contexts.*
import Symbols.*
import Constants.Constant
import ast.Trees.*
import ast.tpd

/** Reads the arguments of the data-class `@data` annotation */
private[plugin] object DataAnnotation {
  import tpd.*

  val name = "dataclass.data"

  def isData(cls: Symbol)(using Context): Boolean =
    cls.annotations.exists(_.symbol.fullName.toString == name)

  /** The tree passed as the `param` argument of the `@data` annotation of `cls`, if any */
  def argument(cls: Symbol, param: String)(using Context): Option[Tree] =
    cls.annotations.iterator
      .filter(_.symbol.fullName.toString == name)
      .flatMap { annot =>
        val args = annot.tree match {
          case Apply(_, args)           => args
          case Block(_, Apply(_, args)) => args
          case _                        => Nil
        }
        val named = args.collectFirst {
          case NamedArg(argName, arg) if argName.toString == param => arg
        }
        // after typing, named arguments are usually reordered into positional ones
        def positional = {
          val paramNames = annot.symbol.primaryConstructor.info.firstParamNames
          val idx        = paramNames.indexWhere(_.toString == param)
          if (idx >= 0) args.lift(idx) else None
        }
        named.orElse(positional)
      }
      .nextOption()

  /** Whether the `@data` annotation of `cls`, if any, sets its `param` boolean argument */
  def isTrue(cls: Symbol, param: String)(using Context): Boolean =
    argument(cls, param).exists {
      case Literal(Constant(true)) => true
      case _                       => false
    }

  /** The `param` string argument of the `@data` annotation of `cls`, if set to a non-null value */
  def string(cls: Symbol, param: String)(using Context): Option[String] =
    argument(cls, param).collect {
      case Literal(Constant(value: String)) => value
    }
}
