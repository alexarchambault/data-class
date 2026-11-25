// SPDX-License-Identifier: MIT

// Local addition, not part of upstream com-lihaoyi/unroll.

package dataclass.plugin

import dotty.tools.dotc.*
import plugins.*
import core.*
import Contexts.*
import Symbols.*
import Flags.*
import Decorators.*
import DenotTransformers.IdentityDenotTransformer
import NameKinds.DefaultGetterName
import Names.*
import StdNames.nme
import Types.*
import Constants.Constant
import ast.Trees.*
import ast.tpd

import scala.language.implicitConversions

/**
 * Implements the `nullOptionBuilderMethodName` argument of the data-class `@data` annotation on
 * case classes, like the data-class macro does on Scala 2.
 *
 * `@data(nullOptionBuilderMethodName = "from") case class Foo(n: Int = 2, s: Option[String])`
 * gets in its companion
 * {{{
 *   def from(n: java.lang.Integer = null, s: String = null): Foo =
 *     new Foo(if (n eq null) 2 else n.intValue, if (s eq null) None else Some(s))
 * }}}
 * `null` stands for the default value of the field, or `None` for fields like `Option[T]` without
 * one, and primitive types are boxed. Fields whose type can't hold `null` (like type parameters
 * of the class), and fields with no default value that aren't options, keep their type and
 * default value.
 *
 * The parameters of the builder get the `@since` annotations of the fields, so that `unroll2`
 * generates its binary-compatible overloads, like those of `apply`. Unlike on Scala 2, where the
 * macro generates them as plain overloads, they are only visible in the bytecode: overloads that
 * callers see prevent implicit conversions to boxed types, like in `from(opt = 3L)`.
 *
 * As callers see the builder, it is added before the pickler. Only code compiled in a later
 * compiler run can call it, not code compiled along with the case class.
 */
class DataClassBuilderPhase extends PluginPhase with IdentityDenotTransformer {
  import tpd.*

  val phaseName = "dataclassBuilder"

  override val runsAfter  = Set(dotty.tools.dotc.transform.PostTyper.name)
  override val runsBefore = Set(dotty.tools.dotc.transform.Pickler.name)

  override def changesMembers: Boolean = true

  private val argumentName = "nullOptionBuilderMethodName"

  /** How the builder method takes a field */
  private sealed abstract class Plan extends Product with Serializable
  /** Same type as the field, with the default value of the field if it has one */
  private case object Kept extends Plan
  /** A type accepting null, standing for the default value of the field (None for options) */
  private final case class Nullable(isOption: Boolean, unbox: Option[TermName]) extends Plan

  private def boxed(using Context): List[(Symbol, String, String)] = List(
    (defn.BooleanClass, "java.lang.Boolean", "booleanValue"),
    (defn.ByteClass, "java.lang.Byte", "byteValue"),
    (defn.ShortClass, "java.lang.Short", "shortValue"),
    (defn.CharClass, "java.lang.Character", "charValue"),
    (defn.IntClass, "java.lang.Integer", "intValue"),
    (defn.LongClass, "java.lang.Long", "longValue"),
    (defn.FloatClass, "java.lang.Float", "floatValue"),
    (defn.DoubleClass, "java.lang.Double", "doubleValue")
  )

  /** The boxed type and unboxing method of a primitive type */
  private def boxedType(tp: Type)(using Context): Option[(Type, TermName)] =
    boxed.collectFirst {
      case (cls, boxedName, unbox) if tp.isRef(cls) =>
        (requiredClassRef(boxedName), termName(unbox))
    }

  private def isTypeParam(tp: Type)(using Context): Boolean = tp match {
    case _: TypeParamRef => true
    case tr: TypeRef     => tr.symbol.is(TypeParam)
    case _               => false
  }

  /**
   * The type a value of type `tp` is passed as, if it can be null, and its unboxing method.
   *
   * Like the data-class macro on Scala 2, type parameters of the class are left alone, as they
   * can be instantiated with primitive types.
   */
  private def nullableType(tp: Type)(using Context): Option[(Type, Option[TermName])] =
    boxedType(tp) match {
      case Some((boxedTp, unbox)) => Some((boxedTp, Some(unbox)))
      case None =>
        val canBeNull =
          !isTypeParam(tp) && !tp.isRef(defn.AnyClass) && (defn.NullType frozen_<:< tp)
        Option.when(canBeNull)((tp, None))
    }

  private def optionElement(tp: Type)(using Context): Option[Type] = tp.dealias match {
    case AppliedType(tycon, List(elem)) if tycon.typeSymbol == defn.OptionClass => Some(elem)
    case _                                                                      => None
  }

  /** The plan for a field of type `tp`, and the type of the corresponding builder parameter */
  private def plan(tp: Type, hasDefault: Boolean)(using Context): (Plan, Type) =
    optionElement(tp) match {
      case Some(elem) =>
        nullableType(elem) match {
          case Some((paramTp, unbox)) => (Nullable(isOption = true, unbox), paramTp)
          case None                   => (Kept, tp)
        }
      case None =>
        nullableType(tp) match {
          case Some((paramTp, unbox)) if hasDefault => (Nullable(isOption = false, unbox), paramTp)
          case _                                    => (Kept, tp)
        }
    }

  override def transformTemplate(tmpl: Template)(using Context): Tree = {
    val module = tmpl.constr.symbol.owner
    if (!module.isClass || !module.is(ModuleClass)) tmpl
    else {
      val cls = module.companionClass
      if (!cls.exists || !cls.is(CaseClass) || !DataAnnotation.isData(cls)) tmpl
      else
        DataAnnotation.string(cls, argumentName).filter(_.nonEmpty) match {
          case Some(name) if !hasHandWritten(module, termName(name)) =>
            cpy.Template(tmpl)(body = tmpl.body ++ builderDefs(cls.asClass, module.asClass, termName(name)))
          case _ => tmpl
        }
    }
  }

  /** Nothing is generated if the companion defines a method with the same name itself */
  private def hasHandWritten(module: Symbol, name: TermName)(using Context): Boolean =
    module.info.decls.lookupAll(name).exists(s => s.is(Method) && !s.is(Synthetic))

  /** The builder method, and the getters of the default values of its parameters */
  private def builderDefs(cls: ClassSymbol, module: ClassSymbol, name: TermName)(using Context): List[Tree] = {
    val ctor     = cls.primaryConstructor
    val ctorInfo = ctor.info
    val fields   = ctor.paramSymss.dropWhile(_.headOption.exists(_.isType)).headOption.getOrElse(Nil)

    def firstClause(tp: Type): MethodType = tp match {
      case pt: PolyType   => firstClause(pt.resType)
      case mt: MethodType => mt
      case other          => throw new MatchError(other)
    }
    val ctorClause = firstClause(ctorInfo)

    val plans: List[(Plan, Type)] =
      ctorClause.paramInfos.zip(fields).map { case (tp, field) => plan(tp, field.is(HasDefault)) }

    def sinceAnnotations(field: Symbol) =
      field.annotations.filter(_.symbol.fullName.toString == "dataclass.since")

    val construct: (List[Type], List[List[Tree]]) => Tree =
      if (DataAnnotation.isTrue(cls, "settersCallApply"))
        DataClassPhase.constructorLikeApply(cls) match {
          case Some(apply) =>
            (targs, argss) => This(module).select(apply).appliedToTypes(targs).appliedToArgss(argss)
          case None => viaNew(cls, ctor)
        }
      else viaNew(cls, ctor)

    def ctorDefault(idx: Int, targs: List[Type]): Tree = {
      val getter = module.info.decl(DefaultGetterName(nme.CONSTRUCTOR, idx)).symbol
      val sel    = This(module).select(getter)
      if (targs.isEmpty) sel else sel.appliedToTypes(targs)
    }

    // the default value of a field, None for options without one
    def fieldDefault(idx: Int, targs: List[Type]): Tree =
      if (fields(idx).is(HasDefault)) ctorDefault(idx, targs)
      else ref(defn.NoneModule)

    val info: Type = {
      def clause(mt: MethodType): MethodType =
        MethodType(mt.paramNames)(_ => plans.map(_._2), _ => mt.resType)
      ctorInfo match {
        case pt: PolyType   => pt.derivedLambdaType(resType = clause(firstClause(pt)))
        case mt: MethodType => clause(mt)
        case other          => throw new MatchError(other)
      }
    }

    val sym = newSymbol(module, name, Method, info, coord = cls.span).enteredAfter(this).asTerm

    val builder = DefDef(
      sym,
      paramss => {
        val (targs, vparamss) = paramss match {
          case tparams :: rest if ctorInfo.isInstanceOf[PolyType] => (tparams.map(_.tpe), rest)
          case _                                                    => (Nil, paramss)
        }
        val args = fields.indices.toList.map { i =>
          val param = vparamss.head(i)
          plans(i)._1 match {
            case Kept => param
            case Nullable(isOption, unbox) =>
              val value  = unbox.fold(param)(u => param.select(u).appliedToNone)
              val isNull = param.ensureConforms(defn.ObjectType).select(defn.Object_eq).appliedTo(nullLiteral)
              if (isOption) If(isNull, fieldDefault(i, targs), someOf(value))
              else If(isNull, ctorDefault(i, targs), value)
          }
        }
        construct(targs, args :: vparamss.tail)
      }
    )
    val params = builder.termParamss.head

    for ((field, param) <- fields.zip(params); annot <- sinceAnnotations(field))
      param.symbol.addAnnotation(annot)

    // `null` for nullable parameters, the default value of the field (or None) for the others
    val getters = plans.zipWithIndex.flatMap {
      case ((Kept, _), i) if !fields(i).is(HasDefault) && optionElement(ctorClause.paramInfos(i)).isEmpty =>
        Nil
      case ((plan, _), i) =>
        val getterInfo = info match {
          case pt: PolyType   => pt.derivedLambdaType(resType = firstClause(pt).paramInfos(i))
          case mt: MethodType => ExprType(mt.paramInfos(i))
          case other          => throw new MatchError(other)
        }
        val getter = newSymbol(module, DefaultGetterName(name, i), Synthetic | Method, getterInfo, coord = cls.span)
          .enteredAfter(this)
          .asTerm
        params(i).symbol.setFlag(HasDefault)
        List(DefDef(
          getter,
          paramss => {
            val targs = paramss.headOption.toList.flatten.map(_.tpe)
            plan match {
              case Nullable(_, _) => nullLiteral
              case Kept           => fieldDefault(i, targs)
            }
          }
        ))
    }
    if (getters.nonEmpty) sym.setFlag(HasDefaultParams)

    builder :: getters
  }

  private def someOf(value: Tree)(using Context): Tree =
    ref(defn.SomeClass.companionModule)
      .select(nme.apply)
      .appliedToType(value.tpe.widen)
      .appliedTo(value)

  private def viaNew(cls: ClassSymbol, ctor: Symbol)(using Context): (List[Type], List[List[Tree]]) => Tree =
    (targs, argss) => New(cls.typeRef).select(ctor).appliedToTypes(targs).appliedToArgss(argss)
}
