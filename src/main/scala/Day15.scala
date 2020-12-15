import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day15 extends App {
  def part1(starting: Seq[Int], turn: Int): Seq[Int] = {
    val turns = ArrayBuffer.empty[Int]
    for (n <- starting) {
      turns.append(n)
    }
    for (t <- starting.length until turn) {
      val l = turns.lastIndexOf(turns.last, turns.length - 2)
      val n = if (l == -1) 0 else t - l - 1
      turns.append(n)
    }
    turns.toSeq
  }

  val starting = args.map(_.toInt)
  println(part1(starting, 2020).last)
}
