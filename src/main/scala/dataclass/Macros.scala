package dataclass

import scala.reflect.macros.whitebox.Context
import scala.annotation.tailrec

private[dataclass] class Macros(val c: Context) extends ImplTransformers {
  import c.universe._

  import dataclass.Macros._

  private val debug = sys.env
    .get("DATACLASS_MACROS_DEBUG")
    .map(_.toBoolean)
    .getOrElse(java.lang.Boolean.getBoolean("dataclass.macros.debug"))

  private class Transformer(
      generateApplyMethods: Boolean,
      generateOptionSetters: Boolean,
      generatedSettersCallApply: Boolean
  ) extends ImplTransformer {
    override def transformClass(
        cdef: ClassDef,
        mdef: ModuleDef
    ): List[ImplDef] =
      cdef match {
        case q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss)
                        extends { ..$earlydefns } with ..$parents { $self =>
                ..$stats
              }""" =>
          val allParams = paramss.flatten
          val tparamsRef = tparams.map { t =>
            tq"${t.name}"
          }

          val hasToString = {
            def fromStats =
              stats.exists {
                case DefDef(_, nme, tparams, vparamss, _, _)
                    if nme.decodedName.toString == "toString" && tparams.isEmpty && vparamss
                      .forall(_.isEmpty) =>
                  true
                case t @ ValDef(_, name, _, _)
                    if name.decodedName.toString == "toString" =>
                  true
                case _ =>
                  false
              }

            val fromFields =
              allParams.exists(_.name.decodedName.toString() == "toString")

            fromFields || fromStats
          }

          val hasHashCode = {
            def fromStats =
              stats.exists {
                case DefDef(_, nme, tparams, vparamss, _, _)
                    if nme.decodedName.toString == "hashCode" && tparams.isEmpty && vparamss
                      .forall(_.isEmpty) =>
                  true
                case t @ ValDef(_, name, _, _)
                    if name.decodedName.toString == "hashCode" =>
                  true
                case _ =>
                  false
              }

            val fromFields =
              allParams.exists(_.name.decodedName.toString() == "hashCode")

            fromFields || fromStats
          }

          val hasTuple = {
            def fromStats =
              stats.exists {
                case DefDef(_, nme, tparams, vparamss, _, _)
                    if nme.decodedName.toString == "tuple" && tparams.isEmpty && vparamss
                      .forall(_.isEmpty) =>
                  true
                case t @ ValDef(_, name, _, _)
                    if name.decodedName.toString == "tuple" =>
                  true
                case _ =>
                  false
              }

            val fromFields =
              allParams.exists(_.name.decodedName.toString() == "tuple")

            fromFields || fromStats
          }

          val namedArgs = paramss.map(_.map { p =>
            q"${p.name}=this.${p.name}"
          })

          val setters = paramss.zipWithIndex.flatMap {
            case (l, groupIdx) =>
              l.zipWithIndex.flatMap {
                case (p, idx) =>
                  val namedArgs0 =
                    namedArgs.updated(
                      groupIdx,
                      namedArgs(groupIdx).updated(idx, q"${p.name}=${p.name}")
                    )
                  val fn = p.name.decodedName.toString.capitalize
                  val withDefIdent = TermName(s"with$fn")

                  def settersCallApply(
                      tpe0: Tree,
                      namedArgs0: List[List[Tree]]
                  ) =
                    if (generatedSettersCallApply)
                      q"def $withDefIdent(${p.name}: $tpe0) = ${tpname.toTermName}[..$tparamsRef](...$namedArgs0)"
                    else
                      q"def $withDefIdent(${p.name}: $tpe0) = new $tpname[..$tparamsRef](...$namedArgs0)"

                  val extraMethods =
                    if (generateOptionSetters) {
                      val wrappedOptionTpe = p.tpt match {
                        case AppliedTypeTree(
                              Ident(TypeName("Option")),
                              List(wrapped)
                            ) =>
                          Seq(wrapped)
                        case _ => Nil
                      }

                      wrappedOptionTpe.map { tpe0 =>
                        val namedArgs0 =
                          namedArgs.updated(
                            groupIdx,
                            namedArgs(groupIdx).updated(
                              idx,
                              q"${p.name}=_root_.scala.Some(${p.name})"
                            )
                          )
                        settersCallApply(tpe0, namedArgs0)
                      }
                    } else
                      Nil

                  settersCallApply(p.tpt, namedArgs0) +: extraMethods
              }
          }

          val wildcardedTparams = tparams.map {
            case TypeDef(mods, name, tparams, rhs) if tparams.isEmpty =>
              tq"$WildcardType"
            case TypeDef(mods, name, tparams, rhs) =>
              val tparams0 = tparams.zipWithIndex.map {
                case (p, n) =>
                  TypeDef(p.mods, TypeName(s"X$n"), p.tparams, p.rhs)
              }
              tq"({type L[..$tparams0]=$WildcardType})#L"
          }

          val equalMethods = {
            val fldChecks = paramss.flatten
              .map { param =>
                q"this.${param.name} == other.${param.name}"
              }
              .foldLeft[Tree](q"true")((a, b) => q"$a && $b")
            Seq(
              q"""
                override def canEqual(obj: Any): _root_.scala.Boolean =
                  obj != null && obj.isInstanceOf[$tpname[..$wildcardedTparams]]
               """,
              q"""
                override def equals(obj: Any): _root_.scala.Boolean =
                  this.eq(obj.asInstanceOf[AnyRef]) || canEqual(obj) && {
                    val other = obj.asInstanceOf[$tpname[..$wildcardedTparams]]
                    $fldChecks
                  }
               """
            )
          }

          val hashCodeMethod =
            if (hasHashCode) Nil
            else {
              val fldLines = allParams
                .map { param =>
                  q"code = 37 * code + this.${param.name}.##"
                }
              Seq(
                q"""override def hashCode: _root_.scala.Int = {
                    var code = 17 + ${tpname.decodedName.toString()}.##
                    ..$fldLines
                    37 * code
                  }
               """
              )
            }

          val tupleMethod =
            if (hasTuple || allParams.lengthCompare(22) > 0) Nil
            else if (allParams.isEmpty)
              Seq(q"private def tuple = ()")
            else {
              val fields = allParams.map(p => q"this.${p.name}")
              val tupleName = TermName(s"Tuple${allParams.length}")
              Seq(
                q"private def tuple = _root_.scala.$tupleName(..$fields)"
              )
            }

          val productMethods = {

            val prodElemNameMethods =
              if (productElemNameAvailable) {
                val elemNameCases = paramss.flatten.zipWithIndex
                  .map {
                    case (param, idx) =>
                      val name = param.name.decodedName.toString
                      cq"$idx => $name"
                  }

                Seq(
                  q"""
                    override def productElementName(n: _root_.scala.Int): _root_.java.lang.String =
                      n match {
                        case ..$elemNameCases
                        case n => throw new _root_.java.lang.IndexOutOfBoundsException(n.toString)
                      }
                   """
                )
              } else
                Nil

            val elemCases = paramss.flatten.zipWithIndex
              .map {
                case (param, idx) =>
                  cq"$idx => this.${param.name}"
              }
            Seq(
              q"""
                override def productPrefix: _root_.java.lang.String =
                  ${tpname.decodedName.toString}
               """,
              q"""
                override def productArity: _root_.scala.Int = ${allParams.length}
               """,
              q"""
                override def productElement(n: _root_.scala.Int): Any =
                  n match {
                    case ..$elemCases
                    case n => throw new _root_.java.lang.IndexOutOfBoundsException(n.toString)
                  }
               """
            ) ++ prodElemNameMethods
          }

          val toStringMethod =
            if (hasToString) Nil
            else {
              val fldLines = allParams.zipWithIndex
                .flatMap {
                  case (param, idx) =>
                    val before =
                      if (idx == 0) Nil
                      else Seq(q"""b.append(", ")""")
                    before :+ q"""b.append(_root_.java.lang.String.valueOf(this.${param.name}))"""
                }
              Seq(
                q"""override def toString: _root_.java.lang.String = {
                    val b = new _root_.java.lang.StringBuilder(${tpname.decodedName
                  .toString() + "("})
                    ..$fldLines
                    b.append(")")
                    b.toString()
                  }
               """
              )
            }

          val splits = {

            @tailrec
            def indexWithAllDefaults(
                l: List[ValDef],
                idx: Int,
                acc: Option[Int]
            ): Option[Int] =
              l match {
                case h :: t =>
                  val next =
                    if (h.rhs.isEmpty) None
                    else acc.orElse(Some(idx))
                  indexWithAllDefaults(t, idx + 1, next)
                case Nil => acc
              }

            val splits0 = Seq(paramss.head.length) ++ indexWithAllDefaults(
              paramss.head,
              0,
              None
            ).toSeq ++
              paramss.head.zipWithIndex
                .filter(
                  _._1.mods.annotations
                    .exists(_.toString().startsWith("new since("))
                )
                .map(_._2)

            splits0.distinct.sorted
          }

          val allParams0 = paramss.map(_.map { p =>
            var flags0 = Flag.PARAMACCESSOR
            if (p.mods.hasFlag(Flag.IMPLICIT))
              flags0 = flags0 | Flag.IMPLICIT
            if (p.mods.hasFlag(Flag.OVERRIDE))
              flags0 = flags0 | Flag.OVERRIDE
            if (p.mods.hasFlag(Flag.PRIVATE) && !p.mods.hasFlag(Flag.LOCAL))
              flags0 = flags0 | Flag.PRIVATE

            // TODO Keep PRIVATE / PROTECTED flags? Warn about them?

            val p0 = {
              val mods0 = Modifiers(
                flags0,
                p.mods.privateWithin,
                p.mods.annotations
              )
              q"$mods0 val ${p.name}: ${p.tpt}"
            }
            val hasSince =
              p0.mods.annotations.exists(_.toString().startsWith("new since("))
            if (hasSince) {
              val mods0 = Modifiers(
                p0.mods.flags,
                p0.mods.privateWithin,
                p0.mods.annotations
                  .filter(!_.toString().startsWith("new since("))
              )
              q"$mods0 val ${p0.name}: ${p0.tpt}"
            } else
              p0
          })

          val extraConstructors = {

            // https://stackoverflow.com/questions/22756542/how-do-i-add-a-no-arg-constructor-to-a-scala-case-class-with-a-macro-annotation/22757936#22757936
            splits.filter(_ != paramss.head.length).foreach { idx =>
              val b = paramss.head.drop(idx)
              // Weirdly enough, things compile fine without this check (resulting in empty trees hanging around)
              b.foreach { p =>
                if (p.rhs.isEmpty)
                  c.abort(
                    p.pos,
                    s"Found parameter with no default value ${p.name} after @since annotation"
                  )
              }
            }

            lazy val newCtorPos = {
              val defaultCtorPos = c.enclosingPosition
              defaultCtorPos
                .withEnd(defaultCtorPos.end + 1)
                .withStart(defaultCtorPos.start + 1)
                .withPoint(defaultCtorPos.point + 1)
            }

            val len = allParams0.head.length
            splits.reverse.filter(_ != len).map { idx =>
              val a = allParams0.head.take(idx)
              val b = paramss.head.drop(idx)
              val a0 = a.map {
                case ValDef(mods, name, tpt, _) =>
                  q"$name: $tpt"
              }
              val a1 = a0 :: allParams0.tail
              atPos(newCtorPos)(q"""
                $ctorMods def this(...$a1) = this(..${a
                .map(p => q"${p.name}") ++ b
                .map(_.rhs)})
              """)
            }
          }

          val mods0 = Modifiers(
            mods.flags.|(Flag.FINAL),
            mods.privateWithin,
            mods.annotations
          )
          val parents0 = parents ::: List(
            tq"_root_.scala.Product",
            tq"_root_.scala.Serializable"
          )
          val res =
            q"""$mods0 class $tpname[..$tparams] $ctorMods(...$allParams0)
                    extends { ..$earlydefns } with ..$parents0 { $self =>
              ..$extraConstructors
              ..$stats
              ..$setters
              ..$toStringMethod
              ..$equalMethods
              ..$hashCodeMethod
              ..$tupleMethod
              ..$productMethods
          }"""

          if (debug)
            System.err.println(res)

          val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" =
            mdef

          val applyMethods =
            if (generateApplyMethods)
              splits.map { idx =>
                val a = allParams0.head.take(idx)
                val b = paramss.head.drop(idx)
                val a0 = a.map {
                  case ValDef(mods, name, tpt, _) =>
                    q"$name: $tpt"
                }
                val a1 = a0 :: paramss.tail
                q""" def apply[..$tparams](...$a1): $tpname[..$tparamsRef] = new $tpname[..$tparamsRef](...${(a
                  .map(p => q"${p.name}") ++ b.map(_.rhs)) :: paramss.tail.map(
                  _.map(p => q"${p.name}")
                )})"""
              }
            else
              Nil

          val mdef0 =
            q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats; ..$applyMethods }"

          if (debug)
            System.err.println(mdef0)

          List(res, mdef0)
        case _ =>
          c.abort(c.enclosingPosition, "@data must annotate a class")
      }
  }

  def impl(annottees: Tree*): Tree = {
    val params = c.prefix.tree match {
      case q"new data()"           => Nil
      case q"new data(..$params0)" => params0
    }

    def warnPublicConstructorParam(): Unit =
      c.warning(
        c.enclosingPosition,
        "publicConstructor parameter of @data annotation has no effect anymore, constructors are now always public"
      )
    params.foreach {
      case q"publicConstructor=true" =>
        warnPublicConstructorParam()
      case q"publicConstructor=false" =>
        warnPublicConstructorParam()
      case _ =>
    }

    val generateApplyMethods = params.forall {
      case q"apply=false" => false
      case _              => true
    }

    val generateOptionSetters = params.exists {
      case q"optionSetters=true" => true
      case _                     => false
    }

    val generatedSettersCallApply = params.exists {
      case q"settersCallApply=true" => true
      case _                        => false
    }

    annottees.transformAnnottees(
      new Transformer(
        generateApplyMethods,
        generateOptionSetters,
        generatedSettersCallApply
      )
    )
  }

}

object Macros {

  // productElementName added in 2.13
  private[dataclass] val productElemNameAvailable = {
    val sv = scala.util.Properties.versionNumberString
    !sv.startsWith("2.11.") && !sv.startsWith("2.12.")
  }

}
