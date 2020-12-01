import scala.io.Source

object Day1 extends App {
  val Array(file, combinationCount) = args

  val ints = Source.fromFile(args.head).getLines.map(_.toInt).toArray
  val found = ints.combinations(combinationCount.toInt).flatMap { vals =>
    if (vals.sum == 2020) Some(vals.product)
    else None
  }
  println(found.toList)
}
