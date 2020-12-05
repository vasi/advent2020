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
  val maxid = positions.map { case(r, c) => 8 * r + c}.max
  println(maxid)
}
