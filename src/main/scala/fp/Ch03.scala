package fp

import scala.annotation.tailrec

object Ch03 {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A] {
    override def toString: String = s"$head :: $tail"
  }

  @tailrec
  object List {
    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(head, tail) => tail
  }

  def setHead[A](xs: List[A], newHead: A) = xs match {
    case Nil => Cons(newHead, Nil)
    case Cons(head, tail) => Cons(newHead, tail)
  }

  @tailrec
  def drop[A](xs: List[A], n: Int): List[A] = xs match {
    case Nil => Nil
    case Cons(head, tail) => if (n == 0) xs else drop(tail, n - 1)
  }

  @tailrec
  def dropWhile[A](xs: List[A])(f: A => Boolean): List[A] = xs match {
    case Nil => Nil
    case Cons(head, tail) => if (f(head)) dropWhile(tail)(f) else xs
  }

  def append[A](xs: List[A], ys: List[A]): List[A] = xs match {
    case Nil => ys
    case Cons(head, tail) => Cons(head, append(tail, ys))
  }

  def init[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(head, tail) => tail match {
      case Nil => Nil
      case Cons(_, _) => Cons(head, init(tail))
    }
  }

  import Numeric.Implicits._
  def sum[A: Numeric](xs: List[A]): A =
    xs match {
      case Nil => implicitly[Numeric[A]].zero
      case Cons(head, tail) => head + sum(tail)
    }
  def product[A: Numeric](xs: List[A]): A =
    xs match {
      case Nil => implicitly[Numeric[A]].one
      case Cons(head, tail) => head * product(tail)
    }

  def foldRight[A, B](xs: List[A], z: B)(f: (A, B) => B): B = xs match {
    case Nil => z
    case Cons(head, tail) => f(head, foldRight(tail, z)(f))
  }

  @tailrec
  def foldLeft[A, B](xs: List[A], z: B)(f: (B, A) => B): B = xs match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  def sum2[A: Numeric](xs: List[A]): A =
    foldLeft(xs, implicitly[Numeric[A]].zero)(_ + _)
  def product2[A: Numeric](xs: List[A]): A =
    foldLeft(xs, implicitly[Numeric[A]].one)(_ * _)

  def length[A](xs: List[A]): Int = xs match {
    case Nil => 0
    case Cons(head, tail) => 1 + length(tail)
  }

  def reverse[A](xs: List[A]): List[A] = foldLeft(xs, Nil: List[A])((z, x) => Cons(x, z))

  def foldRight2[A, B](xs: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(xs), z)((z, x) => f(x, z))

  def append2[A](xs: List[A], ys: List[A]): List[A] = foldRight2(xs, ys)(Cons(_, _))

  def concat[A](xs: List[List[A]]): List[A] = foldRight2(xs, Nil: List[A])(append2)

  def plusOne(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case Cons(head, tail) => Cons(head + 1, plusOne(tail))
  }
  def toString(xs: List[Double]): List[String] = xs match {
    case Nil => Nil
    case Cons(head, tail) => Cons(head.toString, toString(tail))
  }
  def map[A, B](xs: List[A])(f: A => B): List[B] = xs match {
    case Nil => Nil
    case Cons(head, tail) => Cons(f(head), map(tail)(f))
  }
  def filter[A](xs: List[A])(f: A => Boolean): List[A] = xs match {
    case Nil => Nil
    case Cons(head, tail) => if (f(head)) Cons(head, filter(tail)(f)) else filter(tail)(f)
  }
  def flatMap[A](xs: List[A])(f: A => List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(head, tail) => append2(f(head), flatMap(tail)(f))
  }
  def filter2[A](xs: List[A])(f: A => Boolean): List[A] =
    flatMap(xs)(x => if (f(x)) List(x) else Nil)

  def zipPlus[A: Numeric](xs: List[A], ys: List[A]): List[A] = (xs, ys) match {
    case (Nil, Nil) => Nil
    case (Cons(xh, xt), Cons(yh, yt)) => Cons(xh + yh, zipPlus(xt, yt))
    case _ => Nil
  }
  def zipWith[A](xs: List[A], ys: List[A])(f: (A, A) => A): List[A] = (xs, ys) match {
    case (Nil, Nil) => Nil
    case (Cons(xh, xt), Cons(yh, yt)) => Cons(f(xh, yh), zipWith(xt, yt)(f))
    case _ => Nil
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def hasSubSequence[A](xs: List[A], ys: List[A]): Boolean = (xs, ys) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(xh, xt), Cons(yh, yt)) =>
      if (xh == yh) hasSubSequence(xt, yt) else hasSubSequence(xt, ys)
  }

  def size[A](root: Tree[A]): Int = root match {
    case Leaf(value) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }
  def max[A: Numeric](root: Tree[A]): A = root match {
    case Leaf(value) => value
    case Branch(left, right) => implicitly[Numeric[A]].max(max(left), max(right))
  }
  def depth[A: Numeric](root: Tree[A]): Int = root match {
    case Leaf(value) => 1
    case Branch(left, right) => math.max(depth(left), depth(right)) + 1
  }
  def map[A, B](root: Tree[A])(f: A => B): Tree[B] = root match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }
  def fold[A, B](root: Tree[A], z: B)(f1: (B, A) => B)(f2: (B, B) => B): B = root match {
    case Leaf(value) => f1(z, value)
    case Branch(left, right) => f2(fold(left, z)(f1)(f2), fold(right, z)(f1)(f2))
  }
  def size2[A](root: Tree[A]): Int = fold(root, 0)((z, _) => z + 1)(_ + _ + 1)
  def max2[A: Numeric](root: Tree[A]): A =
    fold(root, implicitly[Numeric[A]].zero)((_, x) => x)(implicitly[Numeric[A]].max(_, _))
  def map2[A, B](root: Tree[A])(f: A => B): Tree[B] =
    fold(root, null: Tree[B])((_, x) => Leaf(f(x)))(Branch(_, _))
  def depth2[A](root: Tree[A]): Int = fold(root, 0)((z, _) => 1)(math.max(_, _) + 1)

  def main(args: Array[String]): Unit = {
    println(tail(List(1, 2, 3)))
    println(setHead(List(1, 2, 3), 10))

    println(drop(List(1, 2, 3), 1))
    println(drop(List(1, 2, 3), 2))
    println(drop(List(1, 2, 3), 3))

    println(dropWhile(List(1, 2, 3))(_ <= 1))
    println(dropWhile(List(1, 2, 3))(_ <= 2))
    println(dropWhile(List(1, 2, 3))(_ <= 3))

    println(append(List(1, 2, 3), List(4, 5, 6)))
    println(init(List(1, 2, 3)))

    println(sum(List(1, 2, 3, 4)))
    println(product(List(1, 2, 3, 4)))

    println(foldRight(List(1, 2, 3, 4), 0.0)(_ + _))
    println(foldRight(List(1, 2, 3, 4), 1.0)(_ * _))
    println(foldLeft(List(1, 2, 3, 4), 0.0)(_ + _))
    println(foldLeft(List(1, 2, 3, 4), 1.0)(_ * _))

    println(length(Nil))
    println(length(List(1, 2, 3)))

    println(sum2(List(1, 2, 3, 4)))
    println(product2(List(1, 2, 3, 4)))

    println(reverse(List(1, 2, 3)))

    println(foldRight2(List(1, 2, 3, 4), 0.0)(_ + _))
    println(foldRight2(List(1, 2, 3, 4), 1.0)(_ * _))

    println(append2(List(1, 2, 3), List(4, 5, 6)))

    println(plusOne(List(1, 2, 3)))
    println(toString(List(1.0, 2.0, 3.0)))
    println(map(List(1, 2, 3))(_ + 1))

    println(filter(List(1, 2, 3, 4, 5))(_ % 2 == 0))
    println(flatMap(List(1, 2, 3))(x => List(x, x * 10, x * 100)))
    println(filter2(List(1, 2, 3, 4, 5))(_ % 2 == 0))


    println(zipPlus(List(1, 2, 3), List(4, 5, 6)))
    println(zipPlus(List(1, 2, 3), List(4, 5, 6, 7)))
    println(zipPlus(List(1, 2, 3, 4), List(4, 5, 6)))
    println(zipWith(List(1, 2, 3, 4), List(4, 5, 6))(_ + _))

    println(hasSubSequence(List(1, 2, 3, 4, 5), List(1, 2)))
    println(hasSubSequence(List(1, 2, 3, 4, 5), List(2, 3)))
    println(hasSubSequence(List(1, 2, 3, 4, 5), List(4, 5)))
    println(hasSubSequence(List(1, 2, 3, 4, 5), List(4)))
    println(hasSubSequence(List(1, 2, 3, 4, 5), Nil))
    println(hasSubSequence(List(1, 2, 3, 4, 5), List(5, 6)))

    println(size(Leaf(10)))
    println(size(Branch(Leaf(10), Leaf(20))))
    println(size(Branch(Branch(Leaf(10), Leaf(20)), Leaf(30))))

    println(max(Leaf(10)))
    println(max(Branch(Leaf(10), Leaf(20))))
    println(max(Branch(Branch(Leaf(10), Leaf(20)), Leaf(30))))

    println(depth(Leaf(10)))
    println(depth(Branch(Leaf(10), Leaf(20))))
    println(depth(Branch(Branch(Leaf(10), Leaf(20)), Leaf(30))))

    println(map(Leaf(10))(_ * 1.5))
    println(map(Branch(Leaf(10), Leaf(20)))(_ * 1.5))
    println(map(Branch(Branch(Leaf(10), Leaf(20)), Leaf(30)))(_ * 1.5))

    println(size2(Leaf(10)))
    println(size2(Branch(Leaf(10), Leaf(20))))
    println(size2(Branch(Branch(Leaf(10), Leaf(20)), Leaf(30))))

    println(max2(Leaf(10)))
    println(max2(Branch(Leaf(10), Leaf(20))))
    println(max2(Branch(Branch(Leaf(10), Leaf(20)), Leaf(30))))

    println(depth2(Leaf(10)))
    println(depth2(Branch(Leaf(10), Leaf(20))))
    println(depth2(Branch(Branch(Leaf(10), Leaf(20)), Leaf(30))))

    println(map2(Leaf(10))(_ * 1.5))
    println(map2(Branch(Leaf(10), Leaf(20)))(_ * 1.5))
    println(map2(Branch(Branch(Leaf(10), Leaf(20)), Leaf(30)))(_ * 1.5))
  }

}
