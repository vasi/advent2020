import scala.io.Source

object Day6 extends App {
  val groups: Array[Array[String]] = Util.splitLines(Source.fromFile(args.head).getLines())
  val choices = groups.map { g => g.map(_.chars.toArray.toSet).reduce(_ ++ _).size }.sum
  println(choices)
}
