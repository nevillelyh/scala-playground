import scala.language.higherKinds

object Cats {
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
    //def imap[A, B](fa: F[A])(f: A => B)(fi: B => A): F[B] = map(fa)(f)

    def widen[A, B >: A](fa: F[A]): F[B] = fa.asInstanceOf[F[B]]
    def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)
    def void[A](fa: F[A]): F[Unit] = map(fa)(_ => ())
    def fproduct[A, B](fa: F[A])(f: A => B): F[(A, B)] = map(fa)(a => (a, f(a)))
    def as[A, B](fa: F[A], b: B): F[B] = map(fa)(_ => b)
    def tupleLeft[A, B](fa: F[A], b: B): F[(B, A)] = map(fa)(a => (b, a))
    def tupleRight[A, B](fa: F[A], b: B): F[(A, B)] = map(fa)(a => (a, b))
  }

  trait Applicative[F[_]] extends Functor[F] { F =>
    def pure[A](a: A): F[A]
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

    override def map[A, B](fa: F[A])(f: (A) => B): F[B] = ap(pure(f))(fa)

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = ap(map(fa)(a => (b: B) => (a, b)))(fb)
    def ap2[A, B, Z](ff: F[(A, B) => Z])(fa: F[A], fb: F[B]): F[Z] =
      map(product(fa, product(fb, ff))) { case (a, (b, f)) => f(a, b) }
    def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
      map(product(fa, fb)) { case (a, b) => f(a, b) }

    def replicateA[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))
    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(pure(List.empty[B])) { (a, xs) => map2(f(a), xs)(_ :: _) }
    def sequence[A](as: List[F[A]]): F[List[A]] =
      as.foldRight(pure(List.empty[A])) { (fa, xs) => map2(fa, xs)(_ :: _) }
  }

  trait Monad[F[_]] extends Applicative[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)
    def followedBy[A, B](fa: F[A])(fb: F[B]): F[B] = flatMap(fa)(_ => fb)
    override def ap[A, B](ff: F[(A) => B])(fa: F[A]): F[B] = flatMap(ff)(f => map(fa)(f))
  }
}

object Impls {
  import Cats._
  val optFunctor = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: (A) => B): Option[B] = fa.map(f)
  }
  val optApplicative = new Applicative[Option] {
    override def pure[A](x: A): Option[A] = Some(x)
    override def ap[A, B](ff: Option[(A) => B])(fa: Option[A]): Option[B] =
      for (a <- fa; f <- ff) yield f(a)
  }
  val optMonad = new Monad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] = fa.flatMap(f)
    override def pure[A](x: A): Option[A] = Some(x)
  }
}
