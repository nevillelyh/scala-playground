package fp

import scala.collection.immutable.{::, Nil}

object Ch04 {
  sealed trait Option[+A]
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  def map[A, B](o: Option[A])(f: A => B): Option[B] = o match {
    case Some(get) => Some(f(get))
    case None => None
  }
  def flatMap[A, B](o: Option[A])(f: A => Option[B]): Option[B] = getOrElse(map(o)(f), None)
  def getOrElse[A, B >: A](o: Option[A], default: => B): B = o match {
    case Some(get) => get
    case None => default
  }
  def orElse[A, B >: A](o: Option[A], ob: => Option[B]): Option[B] = getOrElse(map(o)(Some(_)), ob)
  def filter[A](o: Option[A])(f: A => Boolean): Option[A] =
    flatMap(o)(x => if (f(x)) Some(x) else None)

  def variance(xs: Seq[Double]): Option[Double] = {
    val o = if (xs.isEmpty) None else Some(xs.size)
    map(o) { size =>
      val m = xs.sum / size
      xs.map(x => math.pow(x - m, 2)).sum / size
    }
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = o => map(o)(f)
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    flatMap(a)(x => map(b)(y => f(x, y)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case head :: tail => map2(head, sequence(tail))(_ :: _)
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case head :: tail => map2(f(head), traverse(tail)(f))(_ :: _)
  }

  sealed trait Either[+L, +R]
  case class Left[+L](value: L) extends Either[L, Nothing]
  case class Right[+R](value: R) extends Either[Nothing, R]

  def map2[A, B, C, E](a: Either[E, A], b: Either[E, B])
                      (f: (A, B) => C): Either[E, C] = (a, b) match {
    case (Right(x), Right(y)) => Right(f(x, y))
    case (Left(e), _) => Left(e)
    case (_, Left(e)) => Left(e)
  }
  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] = a match {
    case Nil => Right(Nil)
    case head :: tail => map2(head, sequence(tail))(_ :: _)
  }
  def traverse2[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] = a match {
    case Nil => Right(Nil)
    case head :: tail => map2(f(head), traverse2(tail)(f))(_ :: _)
  }

  def main(args: Array[String]): Unit = {
    println(map(Some(1))(_ + "!"))
    println(map(None: Option[Int])(_ + "!"))
    println(flatMap(Some(1))(x => Some(x + "!")))
    println(flatMap(Some(1))(x => None))
    println(flatMap(None: Option[Int])(x => Some(x + "!")))
    println(flatMap(None: Option[Int])(x => None))
    println(getOrElse(Some(1), 10))
    println(getOrElse(None, 10))
    println(orElse(Some(1), Some(10)))
    println(orElse(None, Some(10)))
    println(orElse(None, None))
    println(filter(Some(1))(_ < 0))
    println(filter(Some(1))(_ > 0))

    println(variance(Seq(1, 2, 3)))
    println(variance(Seq.empty))

    println(lift(math.abs)(Some(-1)))
    println(lift(math.abs)(None))

    println(map2(Some(1), Some(2))(_ + _))
    println(map2(Some(1), None: Option[Int])(_ + _))
    println(map2(None: Option[Int], Some(2))(_ + _))

    println(sequence(List(Some(1), Some(2), Some(3))))
    println(sequence(List(Some(1), Some(2), None)))
    println(sequence(List(Some(1), None, Some(3))))
    println(sequence(List(None, Some(2), Some(3))))

    println(traverse(List(1, 2, 3, 4, 5))(x => Some(x + 1)))
    println(traverse(List(1, 2, 3, 4, 5))(x => if (x % 2 == 0) Some(x) else None))

    println(sequence(List(Right(1), Right(2), Right(3))))
    println(sequence(List(Right(1), Right(2), Left("e"))))
    println(sequence(List(Right(1), Left("e"), Right(3))))
    println(sequence(List(Left("e"), Right(2), Right(3))))

    println(traverse2(List(1, 2, 3, 4, 5))(x => Right(x + 1)))
    println(traverse2(List(1, 2, 3, 4, 5))(x => if (x % 2 == 0) Right(x) else Left("e")))
  }
}
