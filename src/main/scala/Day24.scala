import scala.io.Source

object Day24 extends App {
  final val DirRe = """e|se|sw|w|nw|ne""".r
  def parse(file: String): Seq[Seq[String]] = Source.fromFile(file).getLines.toSeq.map { line =>
    DirRe.findAllIn(line).toSeq
  }

  case class Pos(x: Int, y: Int) {
    def go(dir: String): Pos = dir match {
      case "e" => Pos(x + 2, y)
      case "se" => Pos(x + 1, y - 1)
      case "sw" => Pos(x - 1, y - 1)
      case "w" => Pos(x - 2, y)
      case "nw" => Pos(x - 1, y + 1)
      case "ne" => Pos(x + 1, y + 1)
    }
  }
  object Pos {
    def at(dirs: Seq[String]): Pos = {
      var pos = Pos(0, 0)
      dirs.foreach(d => pos = pos.go(d))
      pos
    }
  }

  def part1(directions: Seq[Seq[String]]): Int = {
    directions
      .map(Pos.at)
      .groupBy(identity)
      .values
      .count(_.size % 2 == 1)
  }

  val directions = parse(args.head)
  println(part1(directions))
}
