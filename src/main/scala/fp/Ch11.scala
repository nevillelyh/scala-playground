package fp

import scala.language.{higherKinds, reflectiveCalls}
import scala.util.{Left, Right}

object Ch11 {
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
      (map(fab)(_._1), map(fab)(_._2))
    def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
      case Left(a) => map(a)(Left(_))
      case Right(b) => map(b)(Right(_))
    }
  }
  val listFunctor = new Functor[List] {
    override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa.map(f)
  }

  trait Monad[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    def map[A, B](fa: F[A])(f: (A) => B): F[B] =
      flatMap(fa)(a => unit(f(a)))
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      flatMap(fa)(a => map(fb)(b => f(a, b)))

    def sequence[A](xs: List[F[A]]): F[List[A]] =
      xs.foldRight(unit(List.empty[A])) { (fa, z) =>
        flatMap(fa)(a => map(z)(as => a :: as))
      }
    def traverse[A, B](xs: List[A])(f: A => F[B]): F[List[B]] =
      xs.foldRight(unit(List.empty[B])) { (a, z) =>
        flatMap(f(a))(b => map(z)(bs => b :: bs))
      }
    def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))
    def filterM[A](xs: List[A])(f: A => F[Boolean]): F[List[A]] =
      xs.foldRight(unit(List.empty[A])) { (a, z) =>
        flatMap(f(a)) { b =>
          map(z)(as => if (b) a :: as else as)
        }
      }
    def compose[A, B, C](f: A => F[B])(g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)
    def flatMap2[A, B](fa: F[A])(f: A => F[B]): F[B] = compose({_: Unit => fa})(f)(())
    def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)
    def flatMap3[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
    def compose2[A, B, C](f: A => F[B])(g: B => F[C]): A => F[C] = a => join(map(f(a))(g))
  }

  val optMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)
    override def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] = fa match {
      case Some(a) => f(a)
      case None => None
    }
  }

  case class Id[A](value: A)
  val idMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A, B](fa: Id[A])(f: (A) => Id[B]): Id[B] = f(fa.value)
  }

  case class State[S, A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      State { s =>
        val (a, s1) = run(s)
        (f(a), s1)
      }
    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State { s =>
        val (a, s1) = run(s)
        f(a).run(s1)
      }
  }
  type IntState[A] = State[Int, A]
  val intStateMonad = new Monad[IntState] {
    override def unit[A](a: => A): IntState[A] = State(s => (a, s))
    override def flatMap[A, B](fa: IntState[A])(f: (A) => IntState[B]): IntState[B] =
      fa.flatMap(f)
  }
  val intStateMonad2 = new Monad[({type IntState[A] = State[Int, A]})#IntState] {
    override def unit[A](a: => A): State[Int, A] = State(s => (a, s))
    override def flatMap[A, B](fa: State[Int, A])(f: (A) => State[Int, B]): State[Int, B] =
      fa.flatMap(f)
  }

  def main(args: Array[String]): Unit = {
    println(listFunctor.map(List(1, 2))(_ + 1))
    println(listFunctor.distribute(List(("a", 1))))
    println(listFunctor.codistribute(Left(List("a")): Either[List[String], List[Int]]))
    println(listFunctor.codistribute(Right(List(1)): Either[List[String], List[Int]]))
    println(optMonad.sequence(List(Some(1), Some(2), Some(3), Some(4))))
    println(optMonad.traverse(List(1, 2, 3, 4))(Option(_)))
    println(optMonad.replicateM(10, Some(1)))
    println(optMonad.product(Some("a"), Some(1)))
    println(optMonad.filterM(List(1, 2, 3, 4))(x => Some(x % 2 == 0)))
  }
}
