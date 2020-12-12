import scala.io.Source

object Day12 extends App {
  final val LineRe = """(.)(\d+)""".r
  // axes go up towards NE
  final val Dirs = Seq((1, 0), (0, -1), (-1, 0), (0, 1))

  case class State(x: Int = 0, y: Int = 0, dir: Int = 0)
  def posMod(n: Int, d: Int): Int = {
    val m = n % d
    if (m < 0) m + d
    else m
  }
  def runLine(st: State, line: String): State = {
    line match {
      case LineRe(action, valueStr) =>
        val value = valueStr.toInt
        action match {
          case "N" => st.copy(y = st.y + value)
          case "S" => st.copy(y = st.y - value)
          case "E" => st.copy(x = st.x + value)
          case "W" => st.copy(x = st.x - value)
          case "L" => st.copy(dir = posMod(st.dir - value/90, 4))
          case "R" => st.copy(dir = posMod(st.dir + value/90, 4))
          case "F" =>
            val dir = Dirs(st.dir)
            st.copy(x = st.x + value * dir._1, y = st.y + value * dir._2)
        }
    }
  }
  def manhattan(st: State): Int = st.x.abs + st.y.abs

  val lines = Source.fromFile(args.head).getLines()
  var st = State()
  lines.foreach { line =>
    st = runLine(st, line)
  }
  println(manhattan(st))
}
