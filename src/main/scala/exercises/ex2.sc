// ==================================================
// one/N-hot encoders

// pick a random string from a vocabulary
import scala.util.Random
def randomString(vocabulary: String*) = vocabulary(Random.nextInt(vocabulary.size))
// generate a sequence of random strings from a vocabulary
def randomStrings(n: Int, vocabulary: String*) = Seq.fill(n)(randomString(vocabulary: _*))
val words = randomStrings(100, "apple", "banana", "coconut", "date", "eggplant", "fig", "guava")

// EX: compute unique words, sorted
words.distinct.sorted

// EX: word count
def countWords(words: Seq[String]) = words.map((_, 1)).groupBy(_._1).mapValues(_.size)
val wordCount = countWords(words)

// EX: sort word count by count
wordCount.toList.sortBy(-_._2)
// -x may overflow for Int
Seq(("a", 10), ("b", 5), ("c", 100), ("d", Int.MinValue)).sortBy(-_._2)
// safer alternative
Seq(("a", 10), ("b", 5), ("c", 100), ("d", Int.MinValue)).sortBy(_._2)(Ordering[Int].reverse)

// EX: sort words by length
Seq("apple", "banana", "coconut", "date", "eggplant", "fig", "guava").sortBy(_.length)

// EX: postings, e.g. word -> [p0, p1, ...] where [p0, p1, ...] are indices of word occurrences
words.zipWithIndex.groupBy(_._1).mapValues(_.map(_._2))

// ==================================================

// EX: one hot encoder that converts string labels into binary vectors
// Vocabulary ("a", "b", "c")
// "a" => (1.0, 0.0, 0.0)
// "b" => (0.0, 1.0, 0.0)
// "c" => (0.0, 0.0, 1.0)
// Input: ("a", "b", "b", "c", "b", "b", "a")
// Output:
//   | a b c
// ---------
// a | 1 0 0
// b | 0 1 0
// b | 0 1 0
// c | 0 0 1
// b | 0 1 0
// b | 0 1 0
// a | 1 0 0
def oneHot(in: Seq[String]) = {
  val vocab = in.distinct.sorted
  in.map(s => vocab.map(v => if (v == s) 1.0 else 0.0))
}
val oneHotIn = Seq("a", "b", "c", "a", "a", "a", "b", "b", "c")
val oneHotResult = oneHot(oneHotIn)
(oneHotIn zip oneHotResult).foreach(println)

// ==================================================

// EX: N hot encoder that converts string labels into binary vectors
// Vocabulary ("a", "b", "c")
// "a" => (1.0, 0.0, 0.0)
// "b" => (0.0, 1.0, 0.0)
// "c" => (0.0, 0.0, 1.0)
// Input: (("a", "b"), ("b", "c"), ("b"), ("c"))
// Output:
//     | a b c
// -----------
// a b | 1 1 0
// b c | 0 1 1
// b   | 0 1 0
// c   | 0 0 1

def nHot(in: Seq[Seq[String]]) = {
  val vocab = in.flatten.distinct.sorted
  in.map { row =>
    val s = row.toSet
    vocab.map(v => if (s.contains(v)) 1.0 else 0.0)
  }
}
val nHotIn = Seq(Seq("a"), Seq("b"), Seq("c"), Seq("a", "b"), Seq("b", "c"))
val nHotResult = nHot(nHotIn)
(nHotIn zip nHotResult).foreach(println)
