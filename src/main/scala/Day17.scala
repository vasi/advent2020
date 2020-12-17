import scala.collection.mutable.ArrayBuffer
import scala.io.Source

case class Arena(dims: Seq[Range], bits: Seq[Boolean]) {
  def apply(pos: Seq[Int]): Boolean = {
    var step = 1
    var idx = 0
    dims.zip(pos).foreach { case (dim, p) =>
      if (!dim.contains(p))
        return false
      idx += (p - dim.start) * step
      step *= dim.size
    }
    bits(idx)
  }

  def print(): Unit = {
    def printFlat(chosen: Seq[Int]): Unit = {
      println(s"dims: ${chosen.mkString(", ")}")
      dims(1).foreach { y =>
        val bs = dims(0).map(x => apply(x +: y +: chosen))
        println(bs.map(if (_) "#" else ".").mkString)
      }
      println()
    }
    def helper(chosen: Seq[Int]): Unit = {
      if (dims.size - chosen.size == 2) printFlat(chosen)
      else
        dims(2 + chosen.size).foreach(p => helper(p +: chosen))
    }
    helper(Seq())
  }

  private def grow(range: Range): Range = (range.start - 1) until (range.`end` + 1)

  private def neighborCount(pos: Seq[Int]): Int = {
    def helper(chosen: Seq[Int], todo: Seq[Int]): Int = todo match {
      case head +: tail =>
        (head - 1 to head + 1).map(h => helper(chosen :+ h, tail)).sum
      case _ => if (chosen != pos && apply(chosen)) 1 else 0
    }
    helper(Seq(), pos)
  }

  def next: Arena = {
    def helper(chosen: Seq[Int], dims: Seq[Range]): Seq[Boolean] = dims match {
      case head +: tail =>
        head.map(h => helper(h +: chosen, tail)).reduce(_ ++ _)
      case _ =>
        val self = apply(chosen)
        val neighbors = neighborCount(chosen)
        val b = neighbors == 3 || (self && neighbors == 2)
        Seq(b)
    }
    val ndims = dims.map(r => grow(r))
    val nbits = helper(Seq(), ndims.reverse)
    println(nbits.length -> nbits.count(identity))
    Arena(ndims, nbits)
  }

  def activeCount: Int = bits.count(identity)
}

object Arena {
  def apply(file: String, dimensions: Int): Arena = {
    val lines = Source.fromFile(file).getLines.toSeq
    val w = lines.head.length
    val h = lines.length
    val dims = Seq(0 until w, 0 until h) ++ (1 to dimensions - 2).map(_ => 0 until 1)
    val bits = lines.mkString.toCharArray.map {
      case '#' => true
      case '.' => false
    }
    Arena(dims, bits)
  }
}

object Day17 extends App {
  def run(dims: Int, steps: Int): Int = {
    var arena = Arena(args.head, dims)
//    arena.print()
    for (i <- 1 to steps) {
      arena = arena.next
//      println(s"step $i")
//      arena.print()
    }
    arena.activeCount
  }

  def part1: Int = run(3, 6)
  def part2: Int = run(4, 6)

  println(part2)
}
