import scala.io.Source

object Day6 extends App {
  val groups: Array[Array[String]] = Util.splitLines(Source.fromFile(args.head).getLines())
  val sets = groups.map { g => g.map(_.chars.toArray.toSet) }
  val anyPerGroup = sets.map { g => g.reduce(_ ++ _).size }.sum
  val allPerGroup = sets.map { g => g.reduce(_ & _ ).size }.sum
  println(allPerGroup)
}
