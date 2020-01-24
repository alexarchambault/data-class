# data-class

[![Build Status](https://travis-ci.org/alexarchambault/data-class.svg?branch=master)](https://travis-ci.org/alexarchambault/data-class)
[![Maven Central](https://img.shields.io/maven-central/v/io.github.alexarchambault/data-class_2.13.svg)](https://maven-badges.herokuapp.com/maven-central/io.github.alexarchambault/data-class_2.13)

*data-class* allows to create classes almost like case-classes, but with no
public `unapply` or `copy` methods, making it easier to add fields to them while
maintaining binary compatiblity.

## Usage

### Setup

Add to your `build.sbt`,
```scala
libraryDependencies += "io.github.alexarchambault" %% "data-class" % "0.2.1"
```

The latest version is [![Maven Central](https://img.shields.io/maven-central/v/io.github.alexarchambault/data-class_2.13.svg)](https://maven-badges.herokuapp.com/maven-central/io.github.alexarchambault/data-class_2.13).

The macro paradise plugin is needed up to scala 2.12, and the right
compiler option needs to be used from 2.13 onwards:
```scala
lazy val isAtLeastScala213 = Def.setting {
  import Ordering.Implicits._
  CrossVersion.partialVersion(scalaVersion.value).exists(_ >= (2, 13))
}
libraryDependencies ++= {
  if (isAtLeastScala213.value) Nil
  else Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full))
}
scalacOptions ++= {
  if (isAtLeastScala213.value) Seq("-Ymacro-annotations")
  else Nil
}
```

Lastly, if you know what you are doing, you can manage to have data-class
[be a compile-time only dependency](https://stackoverflow.com/questions/21515325/add-a-compile-time-only-dependency-in-sbt/21516954#21516954).

### API

Use a `@data` annotation instead of a `case` modifier, like
```scala
import dataclass.data

@data class Foo(n: Int, s: String)
```

This annotation adds a number of features, that can also be found in
case classes:
- sensible `equals` / `hashCode` / `toString` implementations,
- `apply` methods in the companion object for easier creation,
- extend the `scala.Product` trait (itself extending `scala.Equal`), and
implement its methods,
- extend the `scala.Serializable` trait.

It also adds things that differ from case classes:
- add `final` modifier to the class,
- for each field, add a corresponding `with` method (field `count: Int`
generates a method `withCount(count: Int)` returning a new instance of the
class with `count` updated).

Most notably, it does _not_ generate `copy` or `unapply` methods, making
binary compatibility much more tractable upon adding new fields (see below).

In the example above, the `@data` macro generates code like the following (modulo macro hygiene):
```scala
final class Foo(val n: Int, val s: String) extends Product with Serializable {

  def withN(n: Int) = new Foo(n = n, s = s)
  def withS(s: String) = new Foo(n = n, s = s)

  override def toString: String = {
    val b = new StringBuilder("Foo(")
    b.append(String.valueOf(n))
    b.append(", ")
    b.append(String.valueOf(s))
    b.append(")")
    b.toString
  }

  override def canEqual(obj: Any): Boolean = obj != null && obj.isInstanceOf[Foo]
  override def equals(obj: Any): Boolean = canEqual(obj) && {
    val other = obj.asInstanceOf[Foo]
    n == other.n && s == other.s
  })

  override def hashCode: Int = {
    var code = 17 + "Foo".##
    code = 37 * code + n.##
    code = 37 * code + s.##
    37 * code
  }

  private def tuple = (this.n, this.s)

  override def productArity: Int = 2
  override def productElement(n: Int): Any = n match {
    case 0 => this.n
    case 1 => this.s
    case n => throw new IndexOutOfBoundsException(n.toString)
  }
}

object Foo {
  def apply(n: Int, s: String): Foo = new Foo(n, s)
}
```

### shapeless

By default, the classes annotated with `@data` now have a shape that
`shapeless.Generic` handles:
```scala
import dataclass.data

@data class Foo(n: Int, d: Double)

import shapeless._
Generic[Foo] // works
```

Note that with shapeless `2.3.3` and prior versions, `Generic` derivation may fail
if the body of the `@data` class contains `val`s or `lazy val`s, see
[shapeless issue #934](https://github.com/milessabin/shapeless/issues/934).

### Adding fields

In order to retain binary compatibility when adding fields, one should:
- annotate the first added field with `dataclass.since`,
- provide default values for the added fields, like
```scala
import dataclass._

@data class Foo(n: Int, d: Double, @since s: String = "", b: Boolean = false)
```

The `@since` annotation makes the `@data` macro generate `apply` methods
compatible with those without the new fields.

The example above generates the following `apply` methods in the companion object of `Foo`:
```scala
object Foo {
  def apply(n: Int, d: Double): Foo = new Foo(n, d, "", false)
  def apply(n: Int, d: Double, s: String, b: Boolean) = new Foo(n, d, s, b)
}
```

The `@since` annotation accepts an optional string argument - a version
can be passed for example - and it can be used multiple times, like
```scala
import dataclass._

@data class Foo(
  n: Int,
  d: Double,
  @since("1.1")
  s: String = "",
  b: Boolean = false,
  @since("1.2")
  count: Option[Int] = None,
  info: Option[String] = None
)
```

This generates the following `apply` methods in the companion object of `Foo`:
```scala
object Foo {
  def apply(n: Int, d: Double): Foo = new Foo(n, d, "", false, None, None)
  def apply(n: Int, d: Double, s: String, b: Boolean) = new Foo(n, d, s, b, None, None)
  def apply(n: Int, d: Double, s: String, b: Boolean, count: Option[Int], info: Option[String]) = new Foo(n, d, s, b, count, info)
}
```

## Related work

- [contraband](https://github.com/sbt/contraband) relies on code generation from
JSON or a custom schema language to generate classes that can be evolved in a
binary compatible way
- [stalagmite](https://gitlab.com/fommil/attic/tree/master/stalagmite) generates
case classes with custom features via some macros (but doesn't aim at helping
maintaining binary compatibility)
