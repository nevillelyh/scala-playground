package fp

import fp.Ch12.Monad

import scala.annotation.tailrec
import scala.language.{higherKinds, reflectiveCalls}

object Ch13 {
  object A {
    trait IO { self =>
      def run: Unit
      def ++(io: IO): IO = new IO {
        override def run: Unit = {
          self.run
          io.run
        }
      }
    }
    object IO {
      def empty: IO = new IO {
        def run: Unit = ()
      }
    }
  }

  object B {
    sealed trait IO[A] { self =>
      def run: A
      def map[B](f: A => B): IO[B] = new IO[B] {
        override def run: B = f(self.run)
      }
      def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
        override def run: B = f(self.run).run
      }
    }
    object IO {
      def unit[A](a: => A): IO[A] = new IO[A] {
        override def run: A = a
      }
      def apply[A](a: => A): IO[A] = unit(a)

      def doWhile[A](a: IO[A])(cond: A => IO[Boolean]): IO[Unit] = for {
        a1 <- a
        ok <- cond(a1)
        _ <- if (ok) doWhile(a)(cond) else unit(())
      } yield ()
      def forever[A](a: IO[A]): IO[A] = {
        lazy val t: IO[A] = forever(a)
        a.flatMap(_ => t)
      }
    }

    def readLine: IO[String] = IO { scala.io.StdIn.readLine }
    def printLine(msg: String): IO[Unit] = IO { println(msg) }

    def converter: IO[Unit] = for {
      _ <- printLine("Enter a temperature in degrees Fahrenheit: ")
      d <- readLine.map(_.toDouble)
      _ <- printLine(((d - 32) * 5.0 / 9.0).toString)
    } yield ()
  }

  object C {
    sealed trait IO[A] {
      def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
      def map[B](f: A => B): IO[B] = flatMap(f.andThen(Return(_)))
    }
    case class Return[A](a: A) extends IO[A]
    case class Suspend[A](resume: () => A) extends IO[A]
    case class FlatMap[A, B](sub: IO[A], f: A => IO[B]) extends IO[B]

    object IO {
      def unit[A](a: => A): IO[A] = Return(a)
      def doWhile[A](a: IO[A])(cond: A => IO[Boolean]): IO[Unit] = for {
        a1 <- a
        ok <- cond(a1)
        _ <- if (ok) doWhile(a)(cond) else unit(())
      } yield ()
      def forever[A](a: IO[A]): IO[A] = {
        lazy val t: IO[A] = forever(a)
        a.flatMap(_ => t)
      }

      def printLine(s: String): IO[Unit] = Suspend(() => Return(println(s)))

      @tailrec def run[A](io: IO[A]): A = io match {
        case Return(a) => a
        case Suspend(r) => r()
        case FlatMap(x, f) => x match {
          case Return(a) => run(f(a))
          case Suspend(r) => run(f(r()))
          case FlatMap(y, g) => run(y.flatMap(a => g(a).flatMap(f)))
        }
      }
    }
  }

  object D {
    sealed trait TailRec[A] {
      def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)
      def map[B](f: A => B): TailRec[B] = flatMap(f andThen (Return(_)))
    }
    case class Return[A](a: A) extends TailRec[A]
    case class Suspend[A](resume: () => A) extends TailRec[A]
    case class FlatMap[A, B](sub: TailRec[A], f: A => TailRec[B]) extends TailRec[B]
  }

  /*
  object E {
    sealed trait Async[A] {
      def flatMap[B](f: A => Async[B]): Async[B] = FlatMap(this, f)
      def map[B](f: A => B): Async[B] = flatMap(f andThen (Return(_)))
    }
    case class Return[A](a: A) extends Async[A]
    case class Suspend[A](resume: () => A) extends Async[A]
    case class FlatMap[A, B](sub: Async[A], f: A => Async[B]) extends Async[B]

    @tailrec def step[A](async: Async[A]): Async[A] = async match {
      case FlatMap(FlatMap(x, f), g) => step(x.flatMap(a => f(a).flatMap(g)))
      case FlatMap(Return(x), f) => step(f(x))
      case _ => async
    }
  }
  */

  object F {
    sealed trait Free[F[_], A]
    case class Return[F[_], A](a: A) extends Free[F, A]
    case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
    case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]
  }

  def main(args: Array[String]): Unit = {
//    B.converter.run
//    val p = B.PrintLine("hello")
//    B.IO.doWhile(p)(_ => B.IO.unit(Random.nextBoolean)).run
//    B.IO.forever(p).run

//    val p = C.IO.printLine("hello")
//    C.IO.run(C.IO.forever(p))
  }
}
