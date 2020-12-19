import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day19 extends App {
  sealed trait Rule {
    def matchLen(rmap: Map[Int, Rule], s: String): Set[Int]
  }
  case class Lit(c: String) extends Rule {
    def matchLen(rmap: Map[Int, Rule], s: String): Set[Int] =
      if (s.startsWith(c)) Set(c.length) else Set.empty
  }
  @tailrec
  case class Sequ(rules: Int*) extends Rule {
    def matchHelper(rmap: Map[Int, Rule], rs: Seq[Rule], s: String): Set[Int] = rs match {
      case Seq() => Set(0)
      case head +: tail =>
        val ps = head.matchLen(rmap, s)
        ps.flatMap { p =>
          matchHelper(rmap, tail, s.slice(p, s.length)).map(p + _)
        }
    }

    override def matchLen(rmap: Map[Int, Rule], s: String): Set[Int] = matchHelper(rmap, rules.map(rmap), s)
  }
  case class Disjoint(or: Sequ*) extends Rule {
    override def matchLen(rmap: Map[Int, Rule], s: String): Set[Int] = or.toSet.flatMap((r: Sequ) => r.matchLen(rmap, s))
  }

  case class Input(rules: Map[Int, Rule], strings: Seq[String]) {
    def matches(rule: Int, s: String): Boolean = rules(rule).matchLen(rules, s).contains(s.length)
    def matchCount(rule: Int): Int = strings.count(s => matches(rule, s))

    def forPart2: Input = copy(rules = rules ++ Map(
      8 -> Disjoint(Sequ(42), Sequ(42, 8)),
      11 -> Disjoint(Sequ(42, 31), Sequ(42, 11, 31))
    ))
  }
  object Input {
    final val RuleRe = """\A(\d+): (.*)\z""".r
    final val LitRe = """"(.*)"""".r

    def parse(file: String): Input = {
      val rules = collection.mutable.Map.empty[Int, Rule]
      val strings = ArrayBuffer.empty[String]
      Source.fromFile(file).getLines.foreach {
        case "" => ()
        case RuleRe(r, s) =>
          rules(r.toInt) = s match {
            case LitRe(l) => Lit(l)
            case _ =>
              val ors = s.split("\\|").map { o =>
                Sequ(o.split("""\s+""").filterNot(_.isEmpty).map(_.toInt): _*)
              }
              Disjoint(ors: _*)
          }
        case s: String => strings.append(s)
      }
      Input(rules.toMap, strings.toSeq)
    }
  }

  val input = Input.parse(args.head)
  println(input.forPart2.matchCount(0))
}
