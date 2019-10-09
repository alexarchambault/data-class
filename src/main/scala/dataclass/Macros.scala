package dataclass

import scala.reflect.macros.whitebox.Context
import scala.annotation.tailrec

private[dataclass] class Macros(val c: Context) extends ImplTransformers {
  import c.universe._

  private val debug = sys.env
    .get("DATACLASS_MACROS_DEBUG")
    .map(_.toBoolean)
    .getOrElse(java.lang.Boolean.getBoolean("dataclass.macros.debug"))

  private class Transformer(publicConstructor: Boolean)
      extends ImplTransformer {
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

          val hasToString = {
            def fromStats = stats.exists {
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
            def fromStats = stats.exists {
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
            def fromStats = stats.exists {
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
              l.zipWithIndex.map {
                case (p, idx) =>
                  val namedArgs0 =
                    namedArgs.updated(
                      groupIdx,
                      namedArgs(groupIdx).updated(idx, q"${p.name}=${p.name}")
                    )
                  val fn = p.name.decodedName.toString.capitalize
                  val withDefIdent = TermName(s"with$fn")
                  q"def $withDefIdent(${p.name}: ${p.tpt}) = new $tpname(...$namedArgs0)"
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
          val tparamsRef = tparams.map { t =>
            tq"${t.name}"
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
                  canEqual(obj) && {
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
            val elemCases = paramss.flatten.zipWithIndex
              .map {
                case (param, idx) =>
                  cq"$idx => this.${param.name}"
              }
            Seq(
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
            )
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

          val ctorMods0 =
            if (publicConstructor) ctorMods
            else
              Modifiers(
                ctorMods.flags.|(Flag.PRIVATE),
                ctorMods.privateWithin,
                ctorMods.annotations
              )

          val extraConstructors = {

            // https://stackoverflow.com/questions/22756542/how-do-i-add-a-no-arg-constructor-to-a-scala-case-class-with-a-macro-annotation/22757936#22757936
            lazy val newCtorPos = {
              val defaultCtorPos = c.enclosingPosition
              defaultCtorPos
                .withEnd(defaultCtorPos.end + 1)
                .withStart(defaultCtorPos.start + 1)
                .withPoint(defaultCtorPos.point + 1)
            }

            val len = paramss.head.length
            splits.reverse.filter(_ != len).map { idx =>
              val (a, b) = paramss.head.splitAt(idx)
              val a0 = a.map {
                case ValDef(mods, name, tpt, _) =>
                  q"$name: $tpt"
              }
              // Weirdly enough, things compile fine without this check (resulting in empty trees hanging around)
              b.foreach { p =>
                if (p.rhs.isEmpty)
                  c.abort(
                    p.pos,
                    s"Found parameter with no default value ${p.name} after @since annotation"
                  )
              }
              val a1 = a0 :: paramss.tail
              atPos(newCtorPos)(q"""
                $ctorMods0 def this(...$a1) = this(..${a
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
          val allParams0 = paramss.map(_.map { p =>
            if (p.mods.hasFlag(Flag.PRIVATE) && p.mods.hasFlag(Flag.LOCAL)) {
              var flags0 = Flag.PARAMACCESSOR
              if (p.mods.hasFlag(Flag.IMPLICIT))
                flags0 = flags0 | Flag.IMPLICIT
              val mods0 = Modifiers(
                flags0,
                p.mods.privateWithin,
                p.mods.annotations
              )
              q"$mods0 val ${p.name}: ${p.tpt}"
            } else
              p
          })
          val parents0 = parents ::: List(
            tq"_root_.scala.Product",
            tq"_root_.scala.Serializable"
          )
          val res =
            q"""$mods0 class $tpname[..$tparams] $ctorMods0(...$allParams0)
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
            if (ctorMods.hasFlag(Flag.PRIVATE)) Nil
            else
              splits.map { idx =>
                val (a, b) = paramss.head.splitAt(idx)
                val a0 = a.map {
                  case ValDef(mods, name, tpt, _) =>
                    q"$name: $tpt"
                }
                // Weirdly enough, things compile fine without this check (resulting in empty trees hanging around)
                b.foreach { p =>
                  if (p.rhs.isEmpty)
                    c.abort(
                      p.pos,
                      s"Found parameter with no default value ${p.name} after @since annotation"
                    )
                }
                val a1 = a0 :: paramss.tail
                q""" def apply[..$tparams](...$a1): $tpname[..$tparamsRef] = new $tpname[..$tparamsRef](...${(a
                  .map(p => q"${p.name}") ++ b.map(_.rhs)) :: paramss.tail.map(
                  _.map(p => q"${p.name}")
                )})"""
              }

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
    val publicConstructor = params.exists {
      case q"publicConstructor=true" => true
    }

    annottees.transformAnnottees(new Transformer(publicConstructor))
  }

}
