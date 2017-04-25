package fp

import fp.Ch11.Functor

import scala.language.{higherKinds, implicitConversions, reflectiveCalls}

object Ch12 {
  trait Applicative[F[_]] extends Functor[F] { F =>
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
    def unit[A](a: => A): F [A]

    def map[A, B](fa: F[A])(f: A => B) = map2(fa, unit(()))((a, _) => f(a))
    def traverse[A, B](xs: List[A])(f: A => F[B]): F[List[B]] =
      xs.foldRight(unit(List.empty[B]))((a, z) => map2(f(a), z)(_ :: _))

    def sequence[A](xs: List[F[A]]): F[List[A]] =
      xs.foldRight(unit(List.empty[A]))((a, z) => map2(a, z)(_ :: _))
    def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

    def product2[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] =
      new Applicative[({type f[x] = (F[x], G[x])})#f] {
        override def unit[A](a: => A): (F[A], G[A]) = (F.unit(a), G.unit(a))
        override def map2[A, B, C](fga: (F[A], G[A]), fgb: (F[B], G[B]))
                                  (f: (A, B) => C): (F[C], G[C]) = {
          val (fa, ga) = fga
          val (fb, gb) = fgb
          (F.map2(fa, fb)(f), G.map2(ga, gb)(f))
        }
      }
    def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
      new Applicative[({type f[x] = F[G[x]]})#f] {
        override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
        override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
          F.map2(fa, fb)((ga, gb) => G.map2(ga, gb)(f))
      }
  }

  trait Applicative2[F[_]] extends Functor[F] {
    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]
    def unit[A](a: => A): F[A]

    def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa)(f.curried))(fb)
    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
      apply(apply(map(fa)(f.curried))(fb))(fc)
    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
      apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)
  }
  trait Monad[F[_]] extends Applicative[F] { F =>
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
    def join[A](ffa: F[F[A]]) = flatMap(ffa)(identity)
    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)
    override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))
    override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      flatMap(fa)(a => map(fb)(b => f(a, b)))
  }

  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)
    override def flatMap[A, B](fa: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] =
      fa.right.flatMap(f)
  }

  sealed trait Validation[+E, +A]
  case class Failure[E](head: E, tail: Vector[E] = Vector.empty) extends Validation[E, Nothing]
  case class Success[A](a: A) extends Validation[Nothing, A]

  def validationApplicative[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])
                              (f: (A, B) => C): Validation[E, C] = (fa, fb) match {
      case (Success(a), Success(b)) => Success(f(a, b))
      case (Success(_), f @ Failure(_, _)) => f
      case (f @ Failure(_, _), Success(_)) => f
      case (Failure(lh, lt), Failure(rh, rt)) => Failure(lh, (lt :+ rh) ++ rt)
    }
    override def unit[A](a: => A): Validation[E, A] = Success(a)
  }

  trait Traverse[F[_]] {
    def traverse[G[_]: Applicative,A, B](fa: F[A])(f: A => G[B]): G[F[B]]
    def sequence[G[_]: Applicative,A](fga: F[G[A]]): G[F[A]] = traverse(fga)(ga => ga)
  }

  val listTraverse = new Traverse[List] {
    override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: (A) => G[B]): G[List[B]] =
      implicitly[Applicative[G]].sequence(fa.map(f))
  }
  val optTraverse = new Traverse[Option] {
    override def traverse[G[_] : Applicative, A, B](fa: Option[A])(f: (A) => G[B]): G[Option[B]] = {
      val F = implicitly[Applicative[G]]
      fa.map(a => F.map(f(a))(Option.apply[B])).getOrElse(F.unit(Option.empty[B]))
    }
  }

  type Const[M, A] = M
  trait Monoid[A] {
    val zero: A
    def plus(x: A, y: A): A
  }
  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[({type f[x] = Const[M, x]})#f] =
    new Applicative[({type f[x] = Const[M, x]})#f] {
      override def unit[A](a: => A): M = M.zero
      override def map2[A, B, C](fa: M, fb: M)(f: (A, B) => C): M = M.plus(fa, fb)
    }
}
