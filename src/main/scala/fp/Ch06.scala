package fp

import scala.annotation.tailrec

object Ch06 {
  trait RNG {
    def nextInt: (Int, RNG)
  }
  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }
  def int(rng: RNG): (Int, RNG) = rng.nextInt
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, newRng) = rng.nextInt
    ((n / 2) - (Int.MinValue / 2), newRng)
  }
  def double(rng: RNG): (Double, RNG) = {
    val (n, newRng) = rng.nextInt
    val double = (n.toDouble - Int.MinValue) / (Int.MaxValue.toDouble - Int.MinValue) * 2 - 1
    (double, newRng)
  }
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((n, d), rng3)
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((n, d), rng2) = intDouble(rng)
    ((d, n), rng2)
  }
  def doubleDoubleDouble(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d, d2, d3), rng4)
  }
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def iter(n: Int, acc: (List[Int], RNG)): (List[Int], RNG) = {
      val (ns, rng) = acc
      if (n == 0) {
        (ns.reverse, rng)
      } else {
        val (i, nextRng) = rng.nextInt
        iter(n - 1, (i :: ns, nextRng))
      }
    }
    iter(count, (Nil, rng))
  }

  type Rand[+A] = RNG => (A, RNG)
  def unit[A](a: A): Rand[A] = rng => (a, rng)
  def map[A, B](r: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = r(rng)
    (f(a), rng2)
  }
  val nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)
  val double2: Rand[Double] = map(nonNegativeInt) { i =>
    (i.toDouble - Int.MinValue) / (Int.MaxValue.toDouble - Int.MinValue) * 2 - 1
  }
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }
  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))
  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng =>
    fs.foldRight((Nil: List[A], rng)) { case (f, (xs, r)) =>
      val (x, r1) = f(r)
      (x :: xs, r1)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = rng => {
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0) (mod, rng2) else nonNegativeLessThan(n)(rng)
  }
  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = r(rng)
    f(a)(rng2)
  }
  def nonNegativeLessThan2(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod > 0) unit(mod) else nonNegativeLessThan2(n)
  }
  def mapA[A, B](r: Rand[A])(f: A => B): Rand[B] = flatMap(r)(f andThen unit)
  def map2A[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => mapA(rb)(b => f(a, b)))
  val rollDie: Rand[Int] = nonNegativeLessThan(6)

  case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] = flatMap(f andThen State.unit[S, B])
    def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
      val (a, s2) = run(s)
      f(a).run(s2)
    }
    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))
  }
  object State {
    def unit[S, A](a: A): State[S, A] = State((a, _))
    def sequence[S, A](xs: List[State[S, A]]): State[S, List[A]] = xs match {
      case Nil => unit(Nil)
      case head :: tail => head.flatMap(h => sequence(tail).map(h :: _))
    }
    def get[S]: State[S, S] = State(s => (s, s))
    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  }

  object Machine {
    sealed trait Input
    case object Coin extends Input
    case object Turn extends Input
    case class Machine(locked: Boolean, candies: Int, coins: Int)

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
      def advance(m: Machine, input: Input) = m match {
        case m @ Machine(locked, candies, coins) => input match {
          case Coin => State.set(Machine(locked = candies < 1, candies, coins + 1))
          case Turn =>
            if (!locked)
              State.set(Machine(locked = true, Math.max(0, candies - 1), coins))
            else
              State.set(m)
        }
      }

      def coinsAndCandies(m: Machine) = m match {
        case Machine(_, candies, coins) => (coins, candies)
      }

      State.sequence(inputs.map(input => State.get.flatMap((m: Machine) => advance(m, input))))
        .flatMap((_: List[Unit]) => State.get.map(coinsAndCandies))
    }
  }

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(42)
    val (n1, rng2) = rng.nextInt
    val (n2, _) = rng2.nextInt
    println(n1)
    println(n2)
  }
}
