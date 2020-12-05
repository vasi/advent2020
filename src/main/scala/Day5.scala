import scala.io.Source

object Day5 extends App {
  def parse(s: String, lo: String, hi: String): Int = {
    Integer.parseInt(s.replace(lo, "0").replace(hi, "1"), 2)
  }

  val passes = Source.fromFile(args.head).getLines
  val positions = passes.map { pass =>
    val (rowstr, colstr) = pass.splitAt(7)
    (parse(rowstr, "F", "B"), parse(colstr, "L", "R"))
  }
  val ids = positions.map { case(r, c) => 8 * r + c}.toSet
  val maxid = ids.max

  val free = (0 to 1023).filter { i =>
    ids.contains(i - 1) && ids.contains(i + 1) && !ids.contains(i)
  }
  println(free.toList)
}
