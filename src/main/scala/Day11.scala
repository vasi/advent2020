import scala.io.Source

object Day11 extends App {
  object Position extends Enumeration {
    type Position = Value
    val Empty, Occupied, Floor = Value
  }
  case class Ferry(state: Seq[Seq[Position.Value]]) {
    def apply(x: Int, y: Int): Position.Value = state(y)(x)
    def nearbyIdxs(x: Int, y: Int): Seq[(Int, Int)] = {
      val all = for {
        dx <- Seq(-1, 0, 1)
        dy <- Seq(-1, 0, 1)
        if dx != 0 || dy != 0
      } yield (x + dx, y + dy)
      all.filter { case (xx, yy) => xx >= 0 && xx < state.head.length && yy >= 0 && yy < state.length}
    }
    def nearbyOccupied(x: Int, y: Int): Int = nearbyIdxs(x, y).map { case (x, y) => apply(x, y) }
      .count(_ == Position.Occupied)

    def next: Ferry = {
      val nextState = state.indices.map { y =>
        state(y).indices.map { x =>
          (apply(x, y), nearbyOccupied(x, y)) match {
            case (Position.Empty, o) if o == 0 => Position.Occupied
            case (Position.Occupied, o) if o >= 4 => Position.Empty
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
  }

  def constantPoint(init: Ferry): Ferry = {
    var f = init
    while (true) {
      val next = f.next
      if (next == f) {
        return f
      }
      f = next
    }
    ???
  }

  val init = Ferry(args.head)
  println(constantPoint(init).allOccupied)
}
