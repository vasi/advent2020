import scala.collection.mutable
import scala.io.Source

object Day10 extends App {
  val adaptors = Source.fromFile(args.head).getLines.map(_.toInt).toSeq
  val sorted = adaptors.sorted
  val diffs = sorted.sliding(2, 1).map { case Seq(a, b) => b - a }
  val allDiffs = Seq(sorted.head) ++ diffs ++ Seq(3)
  val diffCounts = allDiffs.groupBy(identity).view.mapValues(_.size)
  val p1 = diffCounts(1) * diffCounts(3)
//  println(p1)

  def countWays(adaptors: Seq[Int]): Long = {
    val sorted = adaptors.sorted.reverse.appended(0)
    val cache = mutable.Map(sorted.head -> 1L)
    sorted.tail.foreach { n =>
      cache(n) = (1 to 3).map(d => cache.getOrElse(n + d, 0L)).sum
    }
    cache(0)
  }

  val t1 = Seq(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4)
  val t2 = Seq(28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3)
  println(countWays(adaptors))
}
