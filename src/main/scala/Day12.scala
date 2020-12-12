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

  case class State2(x: Int = 0, y: Int = 0, dx: Int = 10, dy: Int = 1)
  def turn(st: State2, degrees: Int): State2 = {
    val turns = posMod(degrees / 90, 4)
    var x = st.dx
    var y = st.dy
    (1 to turns).foreach { _ =>
      val tmp = x
      x = y
      y = -tmp
    }
    st.copy(dx = x, dy = y)
  }

  def runLine2(st: State2, line: String): State2 = {
    line match {
      case LineRe(action, valueStr) =>
        val value = valueStr.toInt
        action match {
          case "N" => st.copy(dy = st.dy + value)
          case "S" => st.copy(dy = st.dy - value)
          case "E" => st.copy(dx = st.dx + value)
          case "W" => st.copy(dx = st.dx - value)
          case "L" => turn(st, -value)
          case "R" => turn(st, value)
          case "F" => st.copy(x = st.x + value * st.dx, y = st.y + value * st.dy)
        }
    }
  }

  val lines = Source.fromFile(args.head).getLines()

  var st = State2()
  lines.foreach { line =>
    st = runLine2(st, line)
    println(st)
  }
  println(st.x.abs + st.y.abs)
}
