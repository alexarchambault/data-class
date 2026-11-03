# data-class Scala 3 plugin

`plugin` is a Scala 3 compiler plugin, forked from the
[com-lihaoyi/unroll](https://github.com/com-lihaoyi/unroll) compiler plugin. It's published
as `io.github.alexarchambault:data-class-plugin_<full Scala version>`, as compiler plugins are
built against exact Scala versions.

It
- generates binary-compatible overloads for the parameters following a `@dataclass.since`
  annotation, like the upstream plugin does for `@com.lihaoyi.unroll`, with the changes listed
  below,
- implements some arguments of the data-class `@data` annotation on case classes, that
  `@data` otherwise ignores on Scala 3.

## Licensing

- The code under `plugin` retains the upstream MIT license, see `plugin/LICENSE`.
- Modifications to it retain the MIT license, so that they can be upstreamed as necessary.

## Provenance

- This code was imported from the vendored copy in
  [coursier/coursier](https://github.com/coursier/coursier) (`modules/unroll/plugin`), itself
  imported from the vendored copy in [chipsalliance/chisel](https://github.com/chipsalliance/chisel),
  itself imported from upstream commit c9fd7e1ce9a92fe4ab15cd12fb61facc3aed8f2c (tag 0.3.0).
- Only the Scala 3 plugin was imported. Its classes were moved from the `unroll` package to
  `dataclass.plugin`, and renamed (`UnrollPluginScala3` to `DataClassPlugin`, `UnrollPhaseScala3`
  to `UnrollPhase`, and so on). The plugin is named `data-class` (`-Xplugin-require:data-class`).
- The plugin handles the data-class `@since` annotation, instead of `@com.lihaoyi.unroll`, so that
  neither the upstream plugin nor its annotation library are needed.
- Local modifications (`UnrollPhase.scala`):
  - only the compiler-generated `apply` / `fromProduct` / `copy` of case classes (flagged
    `Synthetic`) are special-cased, so that hand-written `apply` overloads in case class
    companions no longer make the plugin fail with "wrong number of arguments at unroll2";
  - a forwarder is not generated when an overload with the same signature is defined by hand
    (the shorter forwarders call that overload instead), so that hand-written overloads take
    precedence rather than clashing with generated ones.
- Local addition (`DataClassCopyPhase.scala`): the `copy` of a `@data` case class with type
  parameters keeps the type parameters of the class rather than taking fresh ones, and the
  parameters of a trailing implicit clause get the corresponding field as default value, as the
  data-class macro generates them on Scala 2. Without this, `F` is unconstrained in the
  `def copy[F[_]](…)(implicit sync: Sync[F]): FileCache[F]` Scala 3 generates, and
  `fileCache.copy(location = dir)` fails to find a `Sync[F]`. Since this changes a signature
  callers see, that phase runs before the pickler, unlike the other two. Only code compiled in a
  later compiler run sees the new signature, not code compiled along with the case class.
- Local addition (`DataClassPhase.scala`): implements two arguments of the data-class `@data`
  annotation:
  - `cachedHashCode = true`: the synthetic `hashCode` of the annotated case class is cached in a
    private field, as the Scala 2 macro does;
  - `settersCallApply = true`: the synthetic `copy` (and its unroll forwarders) and `fromProduct`
    create instances via the hand-written companion `apply` with the constructor's signature, as
    the Scala 2 macro's setters and `copy` do.

## Tests

- `test/plugin` holds classes compiled with the plugin, and `test/plugin/test` the tests using
  them.
- `test/plugin-user` is compiled against `test/plugin-proj-v1`, and its tests are run against
  `test/plugin-proj-v2`, which adds `@since` parameters, to check the binary compatibility of
  what the plugin generates.
