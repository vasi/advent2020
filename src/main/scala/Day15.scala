import scala.collection.mutable

object Day15 extends App {
  def part1(starting: Seq[Int], turn: Int): Int = {
    val seen = mutable.Map.empty[Int, Int]
    var last = 0
    for ((n, t) <- starting.zipWithIndex) {
      if (t > 0)
        seen(last) = t
      last = n
    }
    for (t <- starting.length until turn) {
      val n = seen.get(last).fold(0)(t - _)
      seen(last) = t
      last = n
    }
    last
  }

  val starting = args.map(_.toInt)
  println(part1(starting, 30_000_000))
}
