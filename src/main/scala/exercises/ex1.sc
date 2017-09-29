// ==================================================
// basic statistics

// generate a vector of random doubles
import scala.util.Random
Seq.fill(10)(Random.nextDouble())

// EX: Random.nextDouble() => [0.0, 1.0], scale the random double to [-100.0, 100.0]
// Hint: Random.nextInt(n) => [0, n), Random.nextInt(n + 1) => [0, n]
def randomVector(dim: Int) = Seq.fill(dim)(Random.nextDouble() * (Random.nextInt(201) - 100))
val vec = randomVector(10)

// ==================================================

// EX: binarize values to 0.0 or 1.0 with threshold 0.5
vec.map(x => if (x >= 0.5) 1.0 else 0.0)

// ==================================================

// EX: dot product of vector A & B
val vecA = randomVector(10)
val vecB = randomVector(10)
(vecA zip vecB).map(t => t._1 * t._2).sum
// mutable alternative
def dotProd(vecA: Seq[Double], vecB: Seq[Double]) = {
  var r = 0.0
  val (i, j) = (vecA.iterator, vecB.iterator)
  while (i.hasNext && j.hasNext) r += i.next() * j.next()
  r
}

// EX: L2 norm of a vector
math.sqrt(vecA.map(x => x * x).sum)
// mutable alternative
def l2(vector: Seq[Double]) = {
  var r = 0.0
  vector.foreach(x => r += x * x)
  math.sqrt(r)
}

// EX: scale a vector by its L2 norm, verify that the new vector is unit, i.e. L2 norm = 1.0
l2(vec.map(_ / l2(vec)))

// EX: cosine similarity of vector A & B
// dot product (A, B) / (L2 norm(A) * L2 norm(B))
def cosine(a: Seq[Double], b: Seq[Double]) = dotProd(a, b) / (l2(a) * l2(b))
cosine(vecA, vecB)
cosine(vecA, vecA) // same vector
cosine(vecA, vecA.map(_ * -1)) // opposite vector

// ==================================================

// EX: compute min and max
(vec.reduce((x, y) => math.min(x, y)), vec.reduce((x, y) => math.max(x, y)))
def reduce(xs: Seq[Double], f: (Double, Double) => Double) = xs.reduce(f)
(reduce(vec, math.min), reduce(vec, math.max))
(vec.min, vec.max)

// EX: compute min and max at the same time
def minMax(vector: Seq[Double]) =
  vector
    .map(x => (x, x))
    .reduce { (x, y) =>
      (math.min(x._1, y._1), math.max(x._2, y._2))
    }

// EX: scale the vector to between 0.0 and 1.0
def scaleMinMax(vector: Seq[Double]) = {
  val (min, max) = minMax(vector)
  vector.map(x => (x - min) / (max - min))
}
val vecMinMax = scaleMinMax(vec)
(vecMinMax.min, vecMinMax.max)

// ==================================================

// EX: compute min, max, and count at the same time
// Hint: (min, max, count) of 1 element => combine 2 elements
def minMaxCount(vector: Seq[Double]) =
vector
  .map(x => (x, x, 1))
  .reduce { (x, y) =>
    (math.min(x._1, y._1), math.max(x._2, y._2), x._3 + y._3)
  }
minMaxCount(vec)

// EX: compute min, max, count, and sum at the same time
// Hint: (min, max, count, sum) of 1 element => combine 2 elements
def minMaxCountSum(vector: Seq[Double]) =
vector
  .map(x => (x, x, 1, x))
  .reduce { (x, y) =>
    (math.min(x._1, y._1), math.max(x._2, y._2), x._3 + y._3, x._4 + y._4)
  }
minMaxCountSum(vec)

// EX: compute min, max, count, sum and mean at the same time
// Hint: (min, max, count, sum, mean) of 1 element => combine 2 elements
def minMaxCountSumMean(vector: Seq[Double]) =
vector
  .map(x => (x, x, 1, x, x))
  .reduce { (x, y) =>
    val count = x._3 + y._3
    val sum = x._4 + y._4
    (math.min(x._1, y._1), math.max(x._2, y._2), count, sum, sum / count)
  }
minMaxCountSumMean(vec)

// EX: make the code more readable with a Stats case class
case class Stats(min: Double, max: Double, count: Int, sum: Double, mean: Double)
def stats(vector: Seq[Double]) =
  vector
    .map(x => Stats(x, x, 1, x, x))
    .reduce { (x, y) =>
      val count = x.count + y.count
      val sum = x.sum + y.sum
      Stats(math.min(x.min, y.min), math.max(x.max, y.max), count, sum, sum / count)
    }
stats(vec)
(vec.min, vec.max, vec.size, vec.sum, vec.sum / vec.size)
