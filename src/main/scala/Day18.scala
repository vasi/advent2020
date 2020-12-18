import scala.io.Source

object Day18 extends App {
  final val NumRe = """(\d+)""".r
  final val TokRe = """\d+|[+*()]""".r

  def evalNum(it: Iterator[String]): Long = {
    val n = it.next
    n match {
      case "(" => evalSeq(it)
      case NumRe(n) => n.toLong
    }
  }

  def evalSeq(it: Iterator[String]): Long = {
    var acc = evalNum(it)
    while (it.hasNext) {
      val op = it.next()
      if (op == ")") return acc
      val n = evalNum(it)
      op match {
        case "+" => acc += n
        case "*" => acc *= n
      }
    }
    acc
  }

  def evalStr(s: String): Long = {
    evalSeq(TokRe.findAllIn(s).iterator)
  }

  def part1(lines: Iterator[String]): Long = {
    lines.map(evalStr).sum
  }

  val lines = Source.fromFile(args.head).getLines
  println(part1(lines))
}
