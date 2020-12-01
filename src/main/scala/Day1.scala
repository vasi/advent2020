import scala.io.Source

object Day1 extends App {
  val ints = Source.fromFile(args.head).getLines.map(_.toInt).toArray
  val found = ints.combinations(2).flatMap {
    case Array(i, j) if i + j == 2020 => Some(i * j)
    case _ => None
  }
  println(found.toList)
}
