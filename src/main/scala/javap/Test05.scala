package javap

class Test05 {
  def plus1(x: Int, y: Int): Int = ???
  def plus2[T](x: T, y: T): T = ???
  def plus3[@specialized(Int, Long, Float, Double) T](x: T, y: T): T = ???
}
