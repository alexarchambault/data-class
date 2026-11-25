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
 * Implements the `nullOptionBuilderMethodName` and `nullOptionCopyMethodName` arguments of the
 * data-class `@data` annotation on case classes, like the data-class macro does on Scala 2.
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
 * `@data(nullOptionCopyMethodName = "update")` gets a copy-like method in the case class itself,
 * with the parameters of the builder, `null` keeping the current value of the field:
 * {{{
 *   final def update(n: java.lang.Integer = null, s: String = null): Foo =
 *     new Foo(if (n eq null) this.n else n.intValue, if (s eq null) this.s else Some(s))
 * }}}
 * Every field has a current value to fall back to, so that, unlike in the builder, fields with no
 * default value accept `null` too. The parameters of a trailing implicit clause default to the
 * corresponding fields, like those of `copy` (see `DataClassCopyPhase`).
 *
 * As callers see these methods, they are added before the pickler. Only code compiled in a later
 * compiler run can call them, not code compiled along with the case class.
 */
class DataClassNullOptionPhase extends PluginPhase with IdentityDenotTransformer {
  import tpd.*

  val phaseName = "dataclassNullOption"

  override val runsAfter  = Set(dotty.tools.dotc.transform.PostTyper.name)
  override val runsBefore = Set(dotty.tools.dotc.transform.Pickler.name)

  override def changesMembers: Boolean = true

  private val builderArgumentName = "nullOptionBuilderMethodName"
  private val copyArgumentName    = "nullOptionCopyMethodName"

  /** How the generated methods take a field */
  private sealed abstract class Plan extends Product with Serializable
  /** Same type as the field, defaulting to the default (builder) or current (copy) value */
  private case object Kept extends Plan
  /** A type accepting null, standing for the default (builder) or current (copy) value */
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

  private def isDataCaseClass(cls: Symbol)(using Context): Boolean =
    cls.exists && cls.isClass && cls.is(CaseClass) && !cls.is(ModuleClass) && DataAnnotation.isData(cls)

  /** The method name passed as `argument` to the `@data` annotation of `cls`, if any */
  private def methodName(cls: Symbol, argument: String)(using Context): Option[TermName] =
    DataAnnotation.string(cls, argument).filter(_.nonEmpty).map(termName(_))

  override def transformTemplate(tmpl: Template)(using Context): Tree = {
    val owner = tmpl.constr.symbol.owner
    if (!owner.isClass) tmpl
    else if (owner.is(ModuleClass)) {
      val cls = owner.companionClass
      if (!isDataCaseClass(cls)) tmpl
      else
        methodName(cls, builderArgumentName) match {
          case Some(name) if !hasHandWritten(owner, name) =>
            cpy.Template(tmpl)(body = tmpl.body ++ builderDefs(cls.asClass, owner.asClass, name))
          case _ => tmpl
        }
    }
    else if (!isDataCaseClass(owner)) tmpl
    else
      methodName(owner, copyArgumentName) match {
        case Some(name) if !hasHandWritten(owner, name) =>
          cpy.Template(tmpl)(body = tmpl.body ++ copyDefs(owner.asClass, name))
        case _ => tmpl
      }
  }

  /** Nothing is generated if the class or companion defines a method with the same name itself */
  private def hasHandWritten(owner: Symbol, name: TermName)(using Context): Boolean =
    owner.info.decls.lookupAll(name).exists(s => s.is(Method) && !s.is(Synthetic))

  /** The parameters of the first term parameter clause of the constructor of `cls` */
  private def fieldParams(cls: ClassSymbol)(using Context): List[Symbol] =
    cls.primaryConstructor.paramSymss.dropWhile(_.headOption.exists(_.isType)).headOption.getOrElse(Nil)

  private def sinceAnnotations(field: Symbol)(using Context) =
    field.annotations.filter(_.symbol.fullName.toString == "dataclass.since")

  /** Creates an instance, via the hand-written companion `apply` with `settersCallApply` */
  private def construct(cls: ClassSymbol, targs: List[Type], argss: List[List[Tree]])(using Context): Tree = {
    val viaApply =
      if (DataAnnotation.isTrue(cls, "settersCallApply")) DataClassPhase.constructorLikeApply(cls)
      else None
    val fn = viaApply match {
      case Some(apply) => ref(cls.companionModule).select(apply)
      case None        => New(cls.typeRef).select(cls.primaryConstructor)
    }
    fn.appliedToTypes(targs).appliedToArgss(argss)
  }

  private def isNull(param: Tree)(using Context): Tree =
    param.ensureConforms(defn.ObjectType).select(defn.Object_eq).appliedTo(nullLiteral)

  /** The value passed for a field, from a parameter that may be null */
  private def nullableArg(param: Tree, plan: Nullable, ifNull: => Tree)(using Context): Tree = {
    val value = plan.unbox.fold(param)(u => param.select(u).appliedToNone)
    If(isNull(param), ifNull, if (plan.isOption) someOf(value) else value)
  }

  /** The field of `cls` holding the constructor parameter named `name`, if it keeps one */
  private def fieldOf(cls: ClassSymbol, name: Name)(using Context): Option[Symbol] = {
    val field = cls.info.decls.lookup(name)
    Option.when(field.exists && field.isTerm && field.is(ParamAccessor) && !field.is(Method))(field)
  }

  /** The copy-like method, and the getters of the default values of its parameters */
  private def copyDefs(cls: ClassSymbol, name: TermName)(using Context): List[Tree] = {
    val targs = cls.typeParams.map(_.typeRef)
    val clause = cls.primaryConstructor.info match {
      case pt: PolyType   => pt.instantiate(targs).asInstanceOf[MethodType]
      case mt: MethodType => mt
      case other          => throw new MatchError(other)
    }
    val fields = fieldParams(cls)
    val plans  = clause.paramInfos.map(plan(_, hasDefault = true))
    val info   = MethodType(clause.paramNames)(_ => plans.map(_._2), _ => clause.resType)

    def current(field: Symbol): Option[Tree] = fieldOf(cls, field.name).map(This(cls).select(_))

    val sym = newSymbol(cls, name, Method | Final, info, coord = cls.span).enteredAfter(this).asTerm
    val method = DefDef(
      sym,
      paramss => {
        val args = fields.zip(paramss.head).zip(plans).map {
          case ((_, param), (Kept, _))                => param
          case ((field, param), (plan: Nullable, _)) => nullableArg(param, plan, current(field).get)
        }
        construct(cls, targs, args :: paramss.tail)
      }
    )
    val params = method.termParamss.head

    for ((field, param) <- fields.zip(params); annot <- sinceAnnotations(field))
      param.symbol.addAnnotation(annot)

    def getter(idx: Int, tpe: Type, rhs: Tree, param: Symbol) = {
      val getterSym = newSymbol(cls, DefaultGetterName(name, idx), Synthetic | Method, tpe, coord = cls.span)
        .enteredAfter(this)
        .asTerm
      param.setFlag(HasDefault)
      DefDef(getterSym, _ => rhs)
    }

    // `null` for nullable parameters, the current value of the field for the others
    val getters = fields.zip(params).zip(plans).zipWithIndex.flatMap {
      case (((field, param), (plan, _)), i) =>
        val rhs = plan match {
          case Nullable(_, _) => Some(nullLiteral)
          case Kept           => current(field)
        }
        rhs.map(getter(i, ExprType(info.paramInfos(i)), _, param.symbol))
    }

    // like DataClassCopyPhase does for `copy`, the parameters of a trailing implicit clause
    // default to the corresponding fields
    val implicitGetters = info.resType match {
      case implicits: MethodType
          if implicits.isImplicitMethod && !implicits.resType.isInstanceOf[MethodType] =>
        implicits.paramNames.zipWithIndex.flatMap { case (paramName, j) =>
          fieldOf(cls, paramName).map { field =>
            // like any default of a parameter of a later clause, the getter takes the parameters
            // of the earlier ones
            val getterType = MethodType(info.paramNames)(
              g => info.paramInfos.map(_.substParams(info, g.paramRefs)),
              g => implicits.paramInfos(j).substParams(info, g.paramRefs)
            )
            getter(info.paramNames.length + j, getterType, This(cls).select(field), method.termParamss(1)(j).symbol)
          }
        }
      case _ => Nil
    }

    if (getters.nonEmpty || implicitGetters.nonEmpty) sym.setFlag(HasDefaultParams)

    method :: getters ::: implicitGetters
  }

  /** The builder method, and the getters of the default values of its parameters */
  private def builderDefs(cls: ClassSymbol, module: ClassSymbol, name: TermName)(using Context): List[Tree] = {
    val ctor     = cls.primaryConstructor
    val ctorInfo = ctor.info
    val fields   = fieldParams(cls)

    def firstClause(tp: Type): MethodType = tp match {
      case pt: PolyType   => firstClause(pt.resType)
      case mt: MethodType => mt
      case other          => throw new MatchError(other)
    }
    val ctorClause = firstClause(ctorInfo)

    val plans: List[(Plan, Type)] =
      ctorClause.paramInfos.zip(fields).map { case (tp, field) => plan(tp, field.is(HasDefault)) }

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
            case Kept           => param
            case plan: Nullable => nullableArg(param, plan, fieldDefault(i, targs))
          }
        }
        construct(cls, targs, args :: vparamss.tail)
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
}
