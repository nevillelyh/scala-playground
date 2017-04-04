package fp

import scala.annotation.tailrec

object Ch05 {
  sealed trait Stream[+A]
  case object Empty extends Stream[Nothing]
  case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](head: => A, tail: => Stream[A]) = {
      lazy val h = head
      lazy val t = tail
      Cons(() => h, () => t)
    }
    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
  }

  def headOption[A](xs: Stream[A]): Option[A] = xs match {
    case Empty => None
    case Cons(head, _) => Some(head())
  }
  def toList[A](xs: Stream[A]): List[A] = xs match {
    case Empty => Nil
    case Cons(head, tail) => head() :: toList(tail())
  }
  def take[A](xs: Stream[A], n: Int): Stream[A] =
    if (n == 0) Empty else xs match {
      case Empty => Empty
      case Cons(head, tail) => Cons(head, () => take(tail(), n - 1))
    }
  @tailrec
  def drop[A](xs: Stream[A], n: Int): Stream[A] =
    if (n == 0) xs else xs match {
      case Empty => Empty
      case Cons(_, tail) => drop(tail(), n - 1)
    }
  def takeWhile[A](xs: Stream[A])(p: A => Boolean): Stream[A] = xs match {
    case Empty => Empty
    case Cons(head, tail) =>
      if (p(head())) Cons(head, () => takeWhile(tail())(p)) else Empty
  }
  def foldRight[A, B](xs: Stream[A])(z: B)(f: (A, => B) => B): B = xs match {
    case Empty => z
    case Cons(head, tail) => f(head(), foldRight(tail())(z)(f))
  }
  def exists[A](xs: Stream[A])(p: A => Boolean): Boolean = foldRight(xs)(false)((a, b) => p(a) || b)
  def forAll[A](xs: Stream[A])(p: A => Boolean): Boolean = foldRight(xs)(true)((a, b) => p(a) && b)
  def takeWhile2[A](xs: Stream[A])(p: A => Boolean): Stream[A] =
    foldRight(xs)(Stream.empty[A])((a, b) => if (p(a)) Cons(() => a, () => b) else Stream.empty)
  def headOption2[A](xs: Stream[A]): Option[A] =
    foldRight(xs)(None: Option[A])((a, b) => Some(a))
  def map[A, B](xs: Stream[A])(f: A => B): Stream[B] =
    foldRight(xs)(Stream.empty[B])((a, b) => Cons(() => f(a), () => b))
  def filter[A](xs: Stream[A])(f: A => Boolean): Stream[A] =
    foldRight(xs)(Stream.empty[A])((a, b) => if (f(a)) Cons(() => a, () => b) else b)
  def append[A](xs: Stream[A], ys: Stream[A]): Stream[A] =
    foldRight(xs)(ys)((a, b) => Cons(() => a, () => b))
  def flatMap[A, B](xs: Stream[A])(f: A => List[B]): Stream[B] =
    foldRight(xs)(Stream.empty[B])((a, b) => append(Stream(f(a): _*), b))

  def ones: Stream[Int] = Stream.cons(1, ones)
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  import Numeric.Implicits._
  def from[A: Numeric](a: A): Stream[A] = Stream.cons(a, from(a + implicitly[Numeric[A]].one))

  def zipWith[A, B, C](xs: Stream[A], ys: Stream[B])(f: (A, B) => C): Stream[C] = (xs, ys) match {
    case (Cons(xh, xt), Cons(yh, yt)) => Stream.cons(f(xh(), yh()), zipWith(xt(), yt())(f))
    case _ => Empty
  }
  def fibs: Stream[Int] = Stream.cons(0, Stream.cons(1, zipWith(fibs, drop(fibs, 1))(_ + _)))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    case None => Empty
  }

  def from2[A: Numeric](a: A): Stream[A] = unfold(a)(a => Some(a, a + implicitly[Numeric[A]].one))
  def constant2[A](a: A): Stream[A] = unfold(a)(a => Some(a, a))
  def ones2: Stream[Int] = unfold(1)(_ => Some(1, 1))
  def fibs2: Stream[Int] = Stream.cons(0, Stream.cons(1, unfold((0, 1)) { case (a, b) =>
    val c = a + b
    Some((c, (b, c)))
  }))

  def map2[A, B](xs: Stream[A])(f: A => B): Stream[B] = unfold(xs) {
    case Empty => None
    case Cons(head, tail) => Some(f(head()), tail())
  }
  def take2[A](xs: Stream[A], n: Int): Stream[A] = unfold(xs) { z =>
    if (n == 0) None else z match {
      case Empty => None
      case Cons(head, tail) => Some((head(), take(tail(), n - 1)))
    }
  }
  def takeWhile3[A](xs: Stream[A])(p: A => Boolean): Stream[A] = unfold(xs) {
    case Empty => None
    case Cons(head, tail) =>
      val h = head()
      if (p(h)) Some((h, takeWhile3(tail())(p)))
      else None
  }
  def zipWith2[A, B, C](xs: Stream[A], ys: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((xs, ys)) {
      case (Cons(xh, xt), Cons(yh, yt)) => Some((f(xh(), yh()), (xt(), yt())))
      case _ => None
    }
  def zipAll[A, B](xs: Stream[A], ys: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((xs, ys)) {
      case (Cons(xh, xt), Cons(yh, yt)) => Some((Some(xh()), Some(yh())), (xt(), yt()))
      case (Cons(xh, xt), Empty) => Some((Some(xh()), None), (xt(), Empty))
      case (Empty, Cons(yh, yt)) => Some((None, Some(yh())), (Empty, yt()))
      case _ => None
    }
  def startsWith[A](xs: Stream[A], ys: Stream[A]): Boolean =
    forAll(unfold((xs, ys)) {
      case (Cons(xh, xt), Cons(yh, yt)) => Some((xh() == yh(), (xt(), yt())))
      case (_, Empty) => None
      case _ => Some((false, (Empty, Empty)))
    })(identity)
  def tails[A](xs: Stream[A]): Stream[Stream[A]] = unfold(xs) {
    case x: Cons[A] => Some(x, drop(x, 1))
      case _ => None
    }
  def hasSubsequence[A](xs: Stream[A], ys: Stream[A]): Boolean =
    exists(tails(xs))(t => startsWith(t, ys))
  def scanLeft[A, B](xs: Stream[A])(z: B)(f: (A, => B) => B): Stream[B] =
    Stream.cons(z, unfold((xs, z)) {
      case (Cons(head, tail), z0) =>
        val z1 = f(head(), z0)
        Some((z1, (tail(), z1)))
      case (Empty, z0) => None
    })
  def scanRight[A, B](xs: Stream[A])(z: B)(f: (A, => B) => B): Stream[B] = xs match {
    case Empty => Stream(z)
    case Cons(head, tail) =>
      val rest = scanRight(tail())(z)(f)
      Stream.cons(f(head(), headOption(rest).get), rest)
  }

  def main(args: Array[String]): Unit = {
    val fns = (1 to 5).map(x => () => {println("COMPUTE " + x); x})
    val xs = fns.foldRight(Stream.empty[Int])((x, z) => Cons(x, () => z))
    println(headOption(xs))
    println(headOption(xs))
    println(headOption(Stream.empty[Int]))
    println(toList(xs))

    println(toList(take(xs, 5)))
    println(toList(take(xs, 1)))
    println(toList(take(xs, 0)))

    println(toList(drop(xs, 5)))
    println(toList(drop(xs, 1)))
    println(toList(drop(xs, 0)))

    println(toList(takeWhile(xs)(_ <= 3)))
    println(toList(takeWhile(xs)(_ <= 10)))
    println(toList(takeWhile(xs)(_ >= 10)))

    println(exists(xs)(_ <= 3))
    println(exists(xs)(_ <= 10))
    println(exists(xs)(_ >= 10))

    println(forAll(xs)(_ <= 3))
    println(forAll(xs)(_ <= 10))
    println(forAll(xs)(_ >= 10))

    println(toList(takeWhile2(xs)(_ <= 3)))
    println(toList(takeWhile2(xs)(_ <= 10)))
    println(toList(takeWhile2(xs)(_ >= 10)))

    println(headOption2(xs))
    println(headOption2(Stream.empty[Int]))

    println(toList(map(xs)(_ + 10)))
    println(toList(filter(xs)(_ <= 3)))

    println(toList(append(xs, Stream(6, 7, 8, 9, 10))))
    println(toList(flatMap(xs)(x => List(x, x * 10))))

    println(toList(take(ones, 5)))
    println(exists(ones)(_ % 2 != 0))
    println(toList(take(constant(1), 5)))
    println(toList(take(from(1), 5)))

    println(toList(take(zipWith(constant(1), from(100))(_ + _), 10)))
    println(toList(take(fibs, 10)))

    println(toList(take(ones2, 5)))
    println(exists(ones2)(_ % 2 != 0))
    println(toList(take(constant2(1), 5)))
    println(toList(take(from2(1), 5)))
    println(toList(take(fibs2, 10)))

    println(toList(map2(xs)(_ + 10)))
    println(toList(take2(xs, 5)))
    println(toList(take2(xs, 1)))
    println(toList(take2(xs, 0)))
    println(toList(takeWhile3(xs)(_ <= 3)))
    println(toList(takeWhile3(xs)(_ <= 10)))
    println(toList(takeWhile3(xs)(_ >= 10)))
    println(toList(take(zipWith2(constant(1), from(100))(_ + _), 10)))
    println(toList(zipAll(take(from(1), 10), take(from(101), 5))))

    println(startsWith(from(1), take(from(1), 10)))
    println(startsWith(from(1), take(from(2), 10)))
    println(startsWith(take(from(1), 10), from(1)))

    println(toList(map(tails(take(from(1), 5)))(toList)))
    println(hasSubsequence(Stream(1, 2, 3, 4, 5), Stream(2,3,4)))
    println(hasSubsequence(Stream(1, 2, 3, 4, 5), Stream(4, 5, 6)))
    println(hasSubsequence(Stream(1, 2, 3, 4, 5), Stream(0, 1, 2)))

    println(toList(scanLeft(Stream(1, 2, 3))(0)(_ + _)))
    println(toList(scanRight(Stream(1, 2, 3))(0)(_ + _)))
  }
}
