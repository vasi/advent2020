import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.matching.Regex

object Day20 extends App {
  def gridRotateRight(grid: Seq[String]): Seq[String] = grid.indices.map { x =>
    grid.indices.reverse.map(y => grid(y)(x)).mkString
  }

  case class Tile(id: Int, grid: Seq[String]) {
    def buildSides: Seq[String] = Seq(
      grid.head,
      grid.map(_.last).mkString,
      grid.last.reverse,
      grid.map(_.head).reverse.mkString,
    )
    lazy val sides: Seq[String] = buildSides

    def flipVertical: Tile = copy(grid = grid.reverse)
    def rotateRight: Tile = copy(grid = gridRotateRight(grid))

    // Translate so that the given side is in the given position
    // Positions are (top, right, bottom, left)
    def translate(side: String, pos: Int): Tile = {
      if (sides(pos) == side)
        this
      else if (!sides.contains(side))
        flipVertical.translate(side, pos)
      else
        rotateRight.translate(side, pos)
    }

    def innerGrid: Seq[String] = grid.slice(1, grid.length - 1)
      .map(s => s.slice(1, s.length - 1))
  }

  object Tile {
    final val HeaderRe = """\ATile (\d+):\z""".r

    def parse(file: String): Seq[Tile] = {
      val tiles = ArrayBuffer.empty[Tile]
      val grid = ArrayBuffer.empty[String]
      var id = 0

      Source.fromFile(file).getLines.foreach {
        case HeaderRe(i) => id = i.toInt
        case "" =>
          if (grid.nonEmpty)
            tiles += Tile(id, grid.toSeq)
          grid.clear()
        case s => grid.append(s)
      }
      if (grid.nonEmpty)
        tiles += Tile(id, grid.toSeq)
      tiles.toSeq
    }
  }

  case class Placement(tiles: Seq[Tile]) {
    // Map a side to the tile ids that contain it
    type SideMap = Map[String, Seq[Tile]]
    def buildSideMap: SideMap = {
      val sides = tiles.flatMap { tile =>
        tile.sides.flatMap { side =>
          Seq(side, side.reverse).map(_ -> tile)
        }
      }
      sides.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
    }
    lazy val sideMap: SideMap = buildSideMap

    def connection(tile: Tile, side: String): Option[Tile] =
      sideMap(side).find(_.id != tile.id)
    def connectedSides: Map[Tile, Int] = {
      tiles.map { t =>
        t -> t.sides.count(s => connection(t, s).isDefined)
      }.toMap
    }

    def corners: Seq[Tile] = {
      val found = connectedSides.filter(_._2 == 2)
      assert(found.size == 4) // could be less if sides are duplicated!
      found.keys.toSeq
    }

    def placeTopLeft: Tile = {
      var t = connectedSides.find(_._2 == 2).get._1
      // Rotate until it's top-left
      while (Seq(0, 3).exists(i => connection(t, t.sides(i)).isDefined))
        t = t.rotateRight
      t
    }
    // Build a line of tiles, starting from a position on start
    def placeLine(start: Tile, startPos: Int): Seq[Tile] = {
      val buf = ArrayBuffer(start)
      val endPos = (startPos + 2) % 4
      while (true) {
        val startSide = buf.last.sides(startPos)
        val conn = connection(buf.last, startSide)
        conn match {
          case Some(t) =>
            val positioned = t.translate(startSide.reverse, endPos)
            buf.append(positioned)
          case None => return buf.toSeq
        }
      }
      sys.error("should never get here")
    }
    def placeTiles: Seq[Seq[Tile]] = {
      val corner = placeTopLeft
      val left = placeLine(corner, 2)
      left.map(t => placeLine(t, 1))
    }
    def image: Image = {
      val grid = placeTiles.flatMap { tileRow =>
        val inners = tileRow.map(_.innerGrid)
        inners.head.indices.map { y =>
          inners.map(_(y)).mkString
        }
      }
      Image(grid)
    }
  }

  case class Image(grid: Seq[String]) {
    lazy val monster: String  = """                  #
                                  |#    ##    ##    ###
                                  | #  #  #  #  #  #   """.stripMargin
    lazy val monsterRe: Regex = {
      val pattern = monster.replaceAll(" ", ".")
      val lines = pattern.linesIterator.toSeq
      val linelen = lines.map(_.length).max
      val padded = lines.map(l => l + "." * (linelen - l.length))

      val w = grid.head.length
      val sep = "." * (w - linelen)
      // Use positive lookahead, so we can find overlapping matches
      s"(?=${padded.mkString(sep)})".r
    }

    def print(): Unit = grid.foreach(println)

    def hasMonster: Boolean = monsterRe.findFirstIn(grid.mkString).isDefined
    def monsters: Int = monsterRe.findAllIn(grid.mkString).size

    def flipVertical: Image = Image(grid.reverse)
    def rotateRight: Image = Image(gridRotateRight(grid))

    def orient: Image =
      if (hasMonster) this
      else if (flipVertical.hasMonster) flipVertical
      else rotateRight.orient

    def roughness: Int = {
      val ms = orient.monsters
      grid.map(_.count(_ == '#')).sum - ms * monster.count(_ == '#')
    }
  }

  def part1(pl: Placement): Long = pl.corners.map(_.id.toLong).product
  def part2(pl: Placement): Int = pl.image.orient.roughness

  val tiles = Tile.parse(args.head)
  val pl = Placement(tiles)
  println(part2(pl))
}
