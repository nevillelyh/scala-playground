// ==================================================
// search engine with tf-idf ranking

// pick a random string from a vocabulary
import scala.util.Random
def randomString(vocabulary: String*) = vocabulary(Random.nextInt(vocabulary.size))
// generate a sequence of random strings from a vocabulary
def randomStrings(n: Int, vocabulary: String*) = Seq.fill(n)(randomString(vocabulary: _*))
val words = randomStrings(100, "apple", "banana", "coconut", "date", "eggplant", "fig", "guava")
// generate a collection of documents
import java.util.UUID

case class Document(id: String, words: Seq[String])

// fake data
val vocabulary = Seq("apple", "banana", "coconut", "date", "eggplant", "fig", "guava")
val documentsX = Seq.fill(100)(Document(UUID.randomUUID().toString, randomStrings(10, vocabulary: _*)))
val queryX = Seq("apple", "banana")

// real data
// mkdir docs
// gsutil cp "gs://dataflow-samples/shakespeare/*" docs
val path = "file:///Users/neville/dev/scala-playground/docs"
val documents = {
  import java.nio.file.Files
  import java.nio.file.Paths
  import scala.collection.JavaConverters._
  Files.list(Paths.get(new java.net.URI(path))).iterator().asScala.toList.map { p =>
    val words = scala.io.Source.fromFile(p.toFile)
      .getLines()
      .filter(_.length > 20)
      .take(50)
      .flatMap(_.split("[^a-zA-Z']+").filter(_.nonEmpty).map(_.toLowerCase))
    Document(p.getFileName.toString, words.toSeq)
  }
}
val query = Seq("king", "lear")

// EX: compute vocabulary from documents
documents.map(_.words).flatten.distinct
documents.flatMap(_.words).distinct
documents.flatMap(_.words).toSet
// flatMap into desired type, i.e. Set[String]
val docVocab: Set[String] = documents.flatMap(_.words)(scala.collection.breakOut)

// EX: create an inverted index, e.g. word => [docID, ...]
val invertedIdx = documents
  .flatMap(d => d.words.map((_, d.id)))
  .groupBy(_._1)
  .mapValues(_.map(_._2))

// EX: search documents with any one of the query words
// Hint: union of all lists
query.map(q => invertedIdx(q).toSet).reduce(_ ++ _)

// EX: search documents with all one of the query words
// Hint: intersection of all lists
query.map(q => invertedIdx(q).toSet).reduce(_ intersect _)

// Single page tutorial on http://www.tfidf.com/

// EX: compute document frequency, i.e. word -> # of documents with word
val dfMap = documents
  .flatMap(d => d.words.map((_, 1)))
  .groupBy(_._1)
  .mapValues(_.size)

// EX: compute inverse document frequency, i.e. log(N / document frequency)
// N is total number of documents
// plus 1 to prevent
val idfMap = dfMap.mapValues(f => math.log(documents.size.toDouble / f))

// EX: create a frequency weighted inverted index, e.g. word => [(docID, term freq), ...]
val weightedIdx = documents
  .flatMap { d =>
    d.words
      .map((_, 1))
      .groupBy(_._1)
      .mapValues(_.size)
      .map { case (word, count) =>
        (word, (d.id, count.toDouble / d.words.size))
      }
  }
  .groupBy(_._1)
  .mapValues(_.map(_._2))

// EX: search documents and rank by sum of tf-idf
def search(query: Seq[String]) =
  query
    .flatMap { q =>
      val idf = idfMap(q)
      weightedIdx(q).map { case (id, tf) =>
        (id, tf * idf)
      }
    }
    .groupBy(_._1)
    .mapValues(_.map(_._2).sum)
    .toSeq
    .sortBy(_._2)(Ordering[Double].reverse)
    .foreach(println)

search(Seq("king", "lear", "goneril", "regan", "duke", "earl"))
