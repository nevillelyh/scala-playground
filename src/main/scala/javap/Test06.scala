package javap

class Test06 {
  def map(xs: Array[Int], f: Int => Int): Array[Int] = xs.map(f)
  def f(x: Int): Int = ???
  val g = f _
  val h = (x: Int) => x + 1: Int
}
