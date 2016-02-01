def f(a: Boolean, b: => Boolean): Unit = {
  if (!a) println("avoided")
}
def c: Boolean = {
  println("EXPENSIVE FUNCITON!!!@#!@#!@#")
  false
}
f(false, c)