package dataclass.plugintests

// Expands a macro defined in the same compilation run, so that the compiler suspends this unit
// and compiles it in a later pass than the one of Typed
object SuspendedUnit {
  def copied: Typed[String] = Typed("a").copy(n = Macros.one)
}
