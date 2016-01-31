def f(pf: PartialFunction[Any, Int]): Option[Int] = {
  pf.lift(1)
}

f {
  case i: Int => i
  case s: String => s.toInt
}