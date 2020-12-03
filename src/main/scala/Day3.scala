import scala.io.Source

class Trees(file: String) {
  val map: Array[Array[Boolean]] = Source.fromFile(file).getLines.map { line =>
    line.map {
      case '.' => false
      case '#' => true
    }.toArray
  }.toArray

  def width: Int = map.head.length

  def print(): Unit = {
    map.foreach { line =>
      val l = line.map { t =>
        if (t) '.' else '#'
      }.mkString
      println(l)
    }
  }

  def isTree(x: Int, y: Int): Boolean = map(y)(x % width)

  def path(dx: Int, dy: Int): Seq[Boolean] = {
    val steps = map.length / dy
    LazyList.tabulate(steps)(i => isTree(i * dx, i * dy))
  }
}

object Day3 extends App {
  val file = args.head
  val trees = new Trees(file)
  val hit = trees.path(3, 1).count(identity)
  println(hit)
}
