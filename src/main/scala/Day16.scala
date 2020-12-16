import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day16 extends App {
  case class Field(name: String, ranges: Seq[Range])
  case class Ticket(nums: Seq[Int])
  case class Input(fields: Seq[Field], mine: Ticket, other: Seq[Ticket]) {
    def badValue(ticket: Ticket): Option[Int] = {
      ticket.nums.find { n =>
        !fields.exists { f =>
          f.ranges.exists(_.contains(n))
        }
      }
    }

    def part1: Int = other.flatMap(t => input.badValue(t)).sum

  }
  object Input {
    def parse(file: String): Input = {
      val FieldRe = """\A([^:]+): (.+)\z""".r
      val TicketRe = """\A([\d,]+)\z""".r

      val fields = ArrayBuffer.empty[Field]
      var mine = Option.empty[Ticket]
      val other = ArrayBuffer.empty[Ticket]

      val lines = Source.fromFile(file).getLines()
      for (line <- lines) {
        line match {
          case FieldRe(name, value) =>
            val ranges = value.split(" or ").map { r =>
              val endpoints = r.split("-")
              endpoints.head.toInt.to(endpoints.last.toInt)
            }
            fields.append(Field(name, ranges))
          case TicketRe(nums) =>
            val ticket = Ticket(nums.split(",").map(_.toInt))
            if (mine.isEmpty) mine = Some(ticket)
            else other.append(ticket)
          case _ => ()
        }
      }
      Input(fields.toSeq, mine.get, other.toSeq)
    }
  }

  val input = Input.parse(args.head)
  println(input.part1)
}
