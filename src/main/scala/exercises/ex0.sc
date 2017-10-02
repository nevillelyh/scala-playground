// ==================================================

// given Seq[String] a and b, compute all pairs of a + b
// e.g. ("a", "b"), ("x", "y") => ("ax", "ay", "bx", "by")
val a = Seq("a", "b", "c")
val b = Seq("x", "y", "z")

a.map(x => b.map(y => (x + y)))
a.map(x => b.map(x + _))
a.flatMap(x => b.map(x + _))
for (x <- a; y <- b) yield x + y

// what about Seq[Option[String]]?
val a1 = Seq(Some("a"), None, Some("c"))
val b1 = Seq(Some("x"), Some("y"), None)
for (x <- a1; y <- b1) yield if (x.isDefined && y.isDefined) Some(x.get + y.get) else None
for (x <- a1; y <- b1) yield for (i <- x; j <- y) yield i + j
for (x <- a1; y <- b1 if x.isDefined && y.isDefined) yield x.get + y.get

// ==================================================

// given 2 Chars, generate the list of characters in between
def chars(x: Char, y: Char) = (x.toInt to y.toInt).map(_.toChar)
chars('a', 'z')

// generate all alphabetic letters
chars('a', 'z') ++ chars('A', 'Z')

// using Seq('a', 'A') as input?
Seq('a', 'A').flatMap(c => chars(c, (c.toInt + 25).toChar))

// how about alphabetic letters (26 upper, 26 lower) plus 10 digits?
Seq(('a', 26), ('A', 26), ('0', 10)).flatMap { case (c, n) => chars(c, (c.toInt + n - 1).toChar) }

// ==================================================

// merging 2 Map[String, Int] by adding values
val m1 = Map("a" -> 1, "b" -> 2, "c" -> 3)
val m2 = Map("b" -> 20, "c" -> 30, "d" -> 40)

// mutable approach
import scala.collection.mutable
val mm1 = mutable.Map.empty[String, Int]
m1.foreach { case (k, v) => mm1(k) = mm1.getOrElse(k, 0) + v }
m2.foreach { case (k, v) => mm1(k) = mm1.getOrElse(k, 0) + v }
mm1

val mm2 = mutable.Map.empty[String, Int].withDefaultValue(0)
m1.foreach { case (k, v) => mm2(k) += v }
m2.foreach { case (k, v) => mm2(k) += v }
mm2

// inefficient immutable approach
val i = m1.keySet intersect m2.keySet
(m1 -- i) ++ (m2 -- i) ++ i.map(k => k -> (m1(k) + m2(k)))

// also inefficient
(m1.keySet ++ m2.keySet).map { k =>
  k -> (m1.getOrElse(k, 0) + m2.getOrElse(k, 0))
}.toMap

// better
m1 ++ m2.map { case (k, v) => k -> (m1.getOrElse(k, 0) + v) }

// ==================================================

// auto-complete
// given some words, generate a map of prefix -> [word, ...] where prefix is the first 1-5 letters
// e.g. apple, apps, apes, apollo
// Hint: what if w.length > 5? w.substring(0, n) vs w.take(n)
val words = Seq("apple", "apps", "apes", "apollo")

words
  .flatMap { w => (1 to math.min(w.length, 5)).map(n => (w.take(n), w)) }
  .groupBy(_._1)
  .mapValues(_.map(_._2))