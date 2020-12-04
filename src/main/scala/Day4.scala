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

  val hgtRe = """(\d+)(cm|in)""".r
  val eyes = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

  val validators = Map[String, (String) => Boolean](
    "byr" -> (s => s.matches("""\d{4}""") && s >= "1920" && s <= "2002"),
    "iyr" -> (s => s.matches("""\d{4}""") && s >= "2010" && s <= "2020"),
    "eyr" -> (s => s.matches("""\d{4}""") && s >= "2020" && s <= "2030"),
    "hgt" -> {
      case hgtRe(n, u) =>
        if (u == "cm") n.toInt >= 150 && n.toInt <= 193
        else n.toInt >= 59 && n.toInt <= 76
      case _ => false
    },
    "hcl" -> (s => s.matches("""#[a-f0-9]{6}""")),
    "ecl" -> (s => eyes.contains(s)),
    "pid" -> (s => s.matches("""\d{9}""")),
  )
  val required = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  val lines = Source.fromFile(args.head).getLines
  val passports = split(lines, "").map { ls =>
    ls.mkString(" ").split(" ").map { pr =>
      pr.split(":") match {
        case Array(k, v) => (k, v)
      }
    }.toMap
  }
  val haveFields = passports.count(p => validators.keySet.subsetOf(p.keySet))

  val validFields = passports.count { p =>
    validators.forall { case (k, v) =>
      p.get(k).exists(v)
    }
  }
  println(validFields)
}
