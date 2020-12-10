import scala.io.Source

object Day10 extends App {
  val adaptors = Source.fromFile(args.head).getLines.map(_.toInt).toSeq
  val sorted = adaptors.sorted
  val diffs = sorted.sliding(2, 1).map { case Seq(a, b) => b - a }
  val allDiffs = Seq(sorted.head) ++ diffs ++ Seq(3)
  val diffCounts = allDiffs.groupBy(identity).view.mapValues(_.size)
  val p1 = diffCounts(1) * diffCounts(3)
  println(p1)
}
