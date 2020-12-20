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
    val grid = ArrayBuffer.empty[String]
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

  // Map a side to the tile ids that contain it
  type SideMap = Map[String, Seq[Int]]
  def sideMap(tiles: Seq[Tile]): SideMap = {
    val sides = tiles.flatMap { tile =>
      tile.sides.flatMap { side =>
        Seq(side, side.reverse).map(_ -> tile.id)
      }
    }
    sides.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
  }

  def connectedSides(tiles: Seq[Tile]): Map[Tile, Int] = {
    val smap = sideMap(tiles)
    tiles.map { t =>
      t -> t.sides.count(s => smap(s).exists(_ != t.id))
    }.toMap
  }

  def corners(tiles: Seq[Tile]): Seq[Tile] = {
    val found = connectedSides(tiles).filter(_._2 == 2)
    assert(found.size == 4) // could be less if sides are duplicated!
    found.keys.toSeq
  }

  def part1(tiles: Seq[Tile]): Long = corners(tiles).map(_.id.toLong).product

  val tiles = parse(args.head)
  println(part1(tiles))
}
