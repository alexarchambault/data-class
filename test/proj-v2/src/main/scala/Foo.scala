import dataclass._

@data class Foo(
    n: Int,
    private val s: String,
    @since("1.0")
    b: Boolean = false,
    @since("2.0")
    b1: Boolean = true
)
