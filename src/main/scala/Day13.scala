import scala.io.Source

object Day13 extends App {
  def parseInput(file: String): (Int, Array[Option[Int]]) = {
    val lines = Source.fromFile(file).getLines.toArray
    val startTime = lines(0).toInt
    val busLines = lines(1).split(",").map {
      case "x" => None
      case s: String => Some(s.toInt)
    }
    (startTime, busLines)
  }
  def part1(startTime: Int, busLines: Array[Int]): Int = {
    val waits = busLines.map { b => b - (startTime % b)}
    val minWait = waits.minBy(identity)
    val idx = waits.indexOf(minWait)
    val bus = busLines(idx)
    bus * minWait
  }
  def sieveMods(p1: (Long, Long), p2: (Long, Long)): (Long, Long) = {
    var x = p1._2
    while (true) {
      if (x % p2._1 == p2._2) return (p1._1 * p2._1, x)
      x += p1._1
    }
    ???
  }

  def part2(busLines: Array[Option[Int]]): Long = {
    val mods = busLines.indices.flatMap { i =>
      busLines(i).map { b =>
        (b.toLong, Util.posMod(-i, b).toLong)
      }
    }
    val sorted = mods.sortBy(-_._1)
    sorted.reduce(sieveMods)._2
  }

  //  val (startTime, busLines) = (939, Array(7, 13, 59, 31, 19))
  val (startTime, busLines) = parseInput(args.head)
//  println(part1(startTime, busLines.flatten))
//  val testLines = Array(Some(7), Some(13), None, None, Some(59), None, Some(31), Some(19))
  println(part2(busLines))
}
