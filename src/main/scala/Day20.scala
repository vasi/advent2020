import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day20 extends App {
  case class Tile(id: Int, sides: Seq[String])

  def sides(grid: Seq[String]): Seq[String] = Seq(
    grid.head,
    grid.map(_.last).mkString,
    grid.last.reverse,
    grid.map(_.head).reverse.mkString,
  )

  final val HeaderRe = """\ATile (\d+):\z""".r
  def parse(file: String): Seq[Tile] = {
    val tiles = ArrayBuffer.empty[Tile]
    var grid = ArrayBuffer.empty[String]
    var id = 0

    Source.fromFile(file).getLines.foreach {
      case HeaderRe(i) => id = i.toInt
      case "" =>
        if (grid.nonEmpty)
          tiles += Tile(id, sides(grid.toSeq))
        grid.clear()
      case s => grid.append(s)
    }
    if (grid.nonEmpty)
      tiles += Tile(id, sides(grid.toSeq))
    tiles.toSeq
  }

  val tiles = parse(args.head)
  println(tiles)
}
