package javap

class Test07 {
  def plus1(x: Int, y: Int, z: Int = 0): Int = ???
  def plus2(x: Int)(y: Int)(z: Int): Int = ???
  def plus3(x: Int)(implicit y: Int): Int = ???
  def plus4[T](x: T, y: T)(implicit num: Numeric[T]): T = ???
  def plus5[T: Numeric](x: T, y: T): T = ???
}
