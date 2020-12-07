import scala.io.Source
import scala.collection.mutable

case class BagSpec(color: String, count: Int)
case class Rule(outerColor: String, inner: Seq[BagSpec])

object Rule {
  final val RuleRe = """\A(.*) bags contain (.*)\.\z""".r
  final val InnerRe = """(\d+) (.*) bag(?:s?)""".r
  final val InnerNone = "no other bags"

  def apply(line: String): Rule = line match {
    case RuleRe(outer, inner) =>
      val inners = inner.split(", ").flatMap {
        case InnerNone => None
        case InnerRe(num, color) => Some(BagSpec(color, num.toInt))
      }
      Rule(outer, inners)
  }
}

object Day7 extends App {
  def canHold(rules: Seq[Rule], color: String): Set[String] = {
    val colorsSeen = mutable.Set(color)
    var stop = false
    var unusedRules = rules
    while (!stop) {
      val (used, unused) = unusedRules.partition(_.inner.exists(s => colorsSeen.contains(s.color)))
      if (used.isEmpty) {
        stop = true
      } else {
        used.foreach(r => colorsSeen.add(r.outerColor))
        unusedRules = unused
      }
    }
    colorsSeen.toSet - color
  }

  def bagsWithin(rules: Seq[Rule], color: String): Int = {
    def helper(ruleMap: Map[String, Rule], color: String): Int = {
      ruleMap(color).inner.map(i => i.count * (1 + helper(ruleMap, i.color))).sum
    }
    val byColor = rules.map(r => r.outerColor -> r).toMap
    helper(byColor, color)
  }

  val rules = Source.fromFile(args.head).getLines.map(Rule.apply).toSeq
//  println(canHold(rules, "shiny gold").size)
  println(bagsWithin(rules, "shiny gold"))
}
