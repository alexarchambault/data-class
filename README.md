# data-class

*data-class* allows to create case-classes with no `unapply` or `copy` methods,
making it easier to maintain binary compatiblity when adding fields to them.

## Usage

### Setup

Add to your `build.sbt`,
```scala
libraryDependencies += "io.github.alexarchambault" %% "data-class" % "0.0.1"
```

The latest version is [![Maven Central](https://img.shields.io/maven-central/v/io.github.alexarchambault/data-class_2.13.svg)](https://maven-badges.herokuapp.com/maven-central/io.github.alexarchambault/data-class_2.13).

The macro paradise plugin is needed up to scala 2.12, and the right
compiler option needs to be used from 2.13:
```scala
lazy val isScala213 = Def.setting(scalaVersion.value.startsWith("2.13."))
libraryDependencies ++= {
  if (isScala213.value) Nil
  else Seq(compilerPlugin("org.scalamacros" % s"paradise" % "2.1.1" cross CrossVersion.full))
}
scalacOptions ++= {
  if (isScala213.value) Seq("-Ymacro-annotations")
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
- add `scala.Product` (itself extending `scala.Equal`) as parent class, and
implement its methods,
- add `scala.Serializable` as parent class.

It also adds things that differ from case classes:
- add `final` modifier to the class,
- make the constructors `private` by default (can be disabled, see [shapeless section](#shapeless) below),
- for each field, add a corresponding `with` method (field `count: Int`
generates a method `withCount(count: Int)` returning a new instance of the
class with `count` updated).

Most notably, it does _not_ generate `copy` or `unapply` methods, making
binary compatibility much more tractable upon adding new fields (see below).

### shapeless

By default, the classes annotated with `@data` do not have a shape that
`shapeless.Generic` handles. This can be fixed by making the constructors
of the generated classes public, like
```scala
import dataclass.data

@data(publicConstructors=true) class Foo(n: Int, d: Double)

import shapeless._
Generic[Foo] // works
```

### Adding fields

In order to retain binary compatibility when adding fields, one should:
- annotate the first added field with `dataclass.since`,
- provide default values for the added fields, like
```scala
import dataclass._

@data class Foo(n: Int, d: Double, @since s: String = "", b: Boolean = false)
```

The `@since` annotation makes the `@data` macro generate `apply` methods
compatible with those without the new fields. If the constructors
are public, back-compatible constructors are generated too.

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
