import scala.collection.mutable.ArrayBuffer
import scala.io.Source

case class Arena(dims: Seq[Range], bits: Seq[Boolean]) {
  def apply(x: Int, y: Int, z: Int): Boolean = {
    val idx = (z - dims(2).start) * dims(0).size * dims(1).size +
      (y - dims(1).start) * dims(0).size +
      (x - dims(0).start)
    bits(idx)
  }

  def print(): Unit = {
    dims(2).foreach { z =>
      println(s"z=${z}")
      dims(1).foreach { y =>
        val bs = dims(0).map(x => apply(x, y, z))
        println(bs.map(if (_) "#" else ".").mkString)
      }
      println()
    }
  }

  private def grow(range: Range): Range = (range.start - 1) until (range.`end` + 1)

  def contains(x: Int, y: Int, z: Int): Boolean =
    dims(0).contains(x) && dims(1).contains(y) && dims(2).contains(z)

  private def neighborCount(x: Int, y: Int, z: Int): Int = {
    var cnt = 0
    for {
      xx <- x - 1 to x + 1
      yy <- y - 1 to y + 1
      zz <- z - 1 to z + 1
      if xx != x || yy != y || zz != z
    } {
      if (contains(xx, yy, zz) && apply(xx, yy, zz))
        cnt += 1
    }
    cnt
  }

  def next: Arena = {
    val ndims = dims.map(r => grow(r))
    val nbits = ArrayBuffer.empty[Boolean]
    ndims(2).foreach { z =>
      ndims(1).foreach { y =>
        ndims(0).foreach { x =>
          val self = contains(x, y, z) && apply(x, y, z)
          val neighbors = neighborCount(x, y, z)
          val b = neighbors == 3 || (self && neighbors == 2)
          nbits.append(b)
        }
      }
    }
    Arena(ndims, nbits.toSeq)
  }

  def activeCount: Int = bits.count(identity)
}

object Arena {
  def apply(file: String): Arena = {
    val lines = Source.fromFile(file).getLines.toSeq
    val w = lines.head.length
    val h = lines.length
    val dims = Seq(0 until w, 0 until h, 0 until 1)
    val bits = lines.mkString.toCharArray.map {
      case '#' => true
      case '.' => false
    }
    Arena(dims, bits)
  }
}

object Day17 extends App {
  def part1: Int = {
    var arena = Arena(args.head)
    for (i <- 1 to 6) {
      arena = arena.next
    }
    arena.activeCount
  }

  println(part1)
}
