object Test {
  val foo0 = Foo(2, "a")
  val foo1 = Foo(2, "a", false)
  val foo2 = foo1.copy(3, "b")
}
