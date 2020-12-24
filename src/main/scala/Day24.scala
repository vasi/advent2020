import scala.io.Source
import scala.collection.mutable

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

    def neighbors: Seq[Pos] = Pos.Dirs.map(d => go(d))
  }
  object Pos {
    final val Dirs = Seq("e", "se", "sw", "w", "nw", "ne")

    def at(dirs: Seq[String]): Pos = {
      var pos = Pos(0, 0)
      dirs.foreach(d => pos = pos.go(d))
      pos
    }
  }

  class Arena(var black: Set[Pos]) {
    private def minmax(vs: Set[Int]): Range = {
      (vs.min - 1) to (vs.max + 1)
    }

    def becomesBlack(p: Pos): Boolean = {
      val ns = p.neighbors.count(black)
      if (black(p))
        ns == 1 || ns == 2
      else
        ns == 2
    }

    def next(): Unit = {
      val newBlack = for {
        x <- minmax(black.map(_.x))
        y <- minmax(black.map(_.y))
        p = Pos(x, y)
        if becomesBlack(p)
      } yield p
      black = newBlack.toSet
    }

    def nextN(n: Int): Unit = (1 to n).foreach(_ => next())
  }
  object Arena {
    def build(dirs: Seq[Seq[String]]): Arena = {
      val black = directions
        .map(Pos.at)
        .groupBy(identity)
        .filter(_._2.size % 2 == 1)
        .keySet
      new Arena(black)
    }
  }

  val directions = parse(args.head)
  val arena = Arena.build(directions)
  arena.nextN(100)
  println(arena.black.size)
}
