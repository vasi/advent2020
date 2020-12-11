import scala.io.Source

object Day11 extends App {
  object Position extends Enumeration {
    type Position = Value
    val Empty, Occupied, Floor = Value
  }
  type AdjFunc = (Ferry, Int, Int) => Seq[(Int, Int)]
  case class Ferry(state: Seq[Seq[Position.Value]]) {
    def apply(x: Int, y: Int): Position.Value = state(y)(x)
    def valid(x: Int, y: Int): Boolean = x >= 0 && x < state.head.length && y >= 0 && y < state.length

    def nearbyOccupied(x: Int, y: Int, f: AdjFunc): Int = f(this, x, y).count {
      case(nx, ny) => valid(nx, ny) && apply(nx, ny) == Position.Occupied
    }

    def next(adjFunc: AdjFunc, threshold: Int): Ferry = {
      val nextState = state.indices.map { y =>
        state(y).indices.map { x =>
          (apply(x, y), nearbyOccupied(x, y, adjFunc)) match {
            case (Position.Empty, o) if o == 0 => Position.Occupied
            case (Position.Occupied, o) if o >= threshold => Position.Empty
            case (p, _) => p
          }
        }
      }
      Ferry(nextState)
    }

    def allOccupied: Int = state.flatten.count(_ == Position.Occupied)

    def print(): Unit = state.foreach { row =>
      val chars = row.map {
        case Position.Occupied => "#"
        case Position.Floor => "."
        case Position.Empty => "L"
      }.mkString
      println(chars)
    }
  }
  object Ferry {
    def apply(input: String): Ferry = {
      val state = Source.fromFile(input).getLines.toSeq.map { line =>
        line.toCharArray.toSeq.map {
          case 'L' => Position.Empty
          case '#' => Position.Occupied
          case '.' => Position.Floor
        }
      }
      Ferry(state)
    }

    final val directions: Seq[(Int, Int)] = for {
      dx <- Seq(-1, 0, 1)
      dy <- Seq(-1, 0, 1)
      if dx != 0 || dy != 0
    } yield (dx, dy)

    def adjacentIdxs(f: Ferry, x: Int, y: Int): Seq[(Int, Int)] = directions.map { case (dx, dy) =>
      (x + dx, y + dy)
    }

    def visibleIdx(f: Ferry, x: Int, y: Int, dx: Int, dy: Int): Option[(Int, Int)] = {
      var (nx, ny) = (x + dx, y + dy)
      while (f.valid(nx, ny)) {
        val p = f(nx, ny)
        if (p != Position.Floor) return Some((nx, ny))
        nx += dx
        ny += dy
      }
      None
    }

    def visibleIdxs(f: Ferry, x: Int, y: Int): Seq[(Int, Int)] = directions.flatMap { case (dx, dy) =>
      visibleIdx(f, x, y, dx, dy)
    }
   }

  def constantPoint(init: Ferry, adjFunc: AdjFunc, threshold: Int): Ferry = {
    var f = init
    while (true) {
      val next = f.next(adjFunc, threshold)
      if (next == f) {
        return f
      }
      f = next
    }
    ???
  }

  val init = Ferry(args.head)
//  println(constantPoint(init, Ferry.adjacentIdxs, 4).allOccupied)
  println(constantPoint(init, Ferry.visibleIdxs, 5).allOccupied)
}
