import scala.io.Source

object Day2 extends App {
  case class Entry(min: Int, max: Int, letter: Char, password: String) {
    def valid: Boolean = {
      val cnt = password.count(c => c == letter)
      cnt >= min && cnt <= max
    }
  }

  object Entry {
    final val RE = """(\d+)-(\d+) (\w): (\w+)""".r

    def apply(line: String): Entry = line match {
      case RE(min, max, letter, pwd) => Entry(min.toInt, max.toInt, letter.charAt(0), pwd)
    }
  }

  val lines = Source.fromFile(args(0)).getLines
  val valid = lines.map(Entry.apply).filter(_.valid)
  println(valid.size)
}
