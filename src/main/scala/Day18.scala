import scala.io.Source
import scala.collection.BufferedIterator

object Day18 extends App {
  class Evaluator(precedence: Boolean) {
    final val NumRe = """(\d+)""".r
    final val TokRe = """\d+|[+*()]""".r

    def evalNum(it: BufferedIterator[String]): Long = {
      val n = it.next
      n match {
        case "(" =>
          val r = evalSeq(it)
          assert(it.next() == ")")
          r
        case NumRe(n) => n.toLong
      }
    }

    def evalSeq(it: BufferedIterator[String]): Long = {
      var acc = evalNum(it)
      while (it.hasNext) {
        if (it.head == ")")
          return acc
        val n = it.next()
        n match {
          case "+" =>
            val m = evalNum(it)
            acc += m
          case "*" =>
            val m = if (precedence) evalSeq(it) else evalNum(it)
            acc *= m
        }
      }
      acc
    }

    def evalStr(s: String): Long = {
      evalSeq(TokRe.findAllIn(s).iterator.buffered)
    }
  }

  def run(lines: Iterator[String], precedence: Boolean): Long = {
    val ev = new Evaluator(precedence = precedence)
    lines.map(ev.evalStr).sum
  }

  val lines = Source.fromFile(args.head).getLines
  println(run(lines, precedence = true))
}
