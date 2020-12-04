import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day4 extends App {
  def split(it: Iterator[String], sep: String): Array[Array[String]] = {
    val buf = new ArrayBuffer[Array[String]]
    var rem = it
    while (rem.nonEmpty) {
      val (pre, post) = it.span(!_.isEmpty)
      buf.append(pre.toArray)
      rem = post.drop(1)
    }
    buf.toArray
  }

  val required = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  val lines = Source.fromFile(args.head).getLines
  val passports = split(lines, "").map { ls =>
    ls.mkString(" ").split(" ").map(_.split(":").head).toSet
  }
  println(passports.count(required.subsetOf(_)))
}
