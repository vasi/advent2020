import scala.io.Source

object Day13 extends App {
  def parseInput(file: String): (Int, Array[Int]) = {
    val lines = Source.fromFile(file).getLines.toArray
    val startTime = lines(0).toInt
    val busLines = lines(1).split(",").filter(_ != "x").map(_.toInt)
    (startTime, busLines)
  }
  def part1(startTime: Int, busLines: Array[Int]): Int = {
    val waits = busLines.map { b => b - (startTime % b)}
    val minWait = waits.minBy(identity)
    val idx = waits.indexOf(minWait)
    val bus = busLines(idx)
    bus * minWait
  }

  //  val (startTime, busLines) = (939, Array(7, 13, 59, 31, 19))
  val (startTime, busLines) = parseInput(args.head)
  println(part1(startTime, busLines))
}
