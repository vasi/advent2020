import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day16 extends App {
  case class Field(name: String, ranges: Seq[Range]) {
    def matches(n: Int): Boolean = ranges.exists(_.contains(n))
  }
  case class Ticket(nums: Seq[Int])
  case class Input(fields: Seq[Field], mine: Ticket, other: Seq[Ticket]) {
    def badValue(ticket: Ticket): Option[Int] = {
      ticket.nums.find { n =>
        !fields.exists(_.matches(n))
      }
    }
    def part1: Int = other.flatMap(t => input.badValue(t)).sum

    def validTickets: Seq[Ticket] = other.filter(t => badValue(t).isEmpty).appended(mine)
    def fieldOrder: Seq[Field] = {
      val validTix = validTickets
      val validFields = mine.nums.indices.map { i =>
        fields.filter { f =>
          validTix.forall(t => f.matches(t.nums(i)))
        }.to(collection.mutable.Set)
      }

      val actualFields = ArrayBuffer.fill[Option[Field]](mine.nums.length)(None)
      while (actualFields.exists(_.isEmpty)) {
        // Find a field that has only one option
        val idx = validFields.indexWhere(_.size == 1)
        if (idx == -1) sys.error("can't find decided field")
        val field = validFields(idx).head
        actualFields(idx) = Some(field)
        validFields.foreach(_.remove(field))
      }

      actualFields.map(_.get).toSeq
    }
    def ticketFields(ticket: Ticket): Map[String, Int] =
      fieldOrder.zipWithIndex.map(f => f._1.name -> ticket.nums(f._2)).toMap

    def part2: Long = {
      val departures = ticketFields(mine).filter(_._1.startsWith("departure ")).values
      assert(departures.size == 6)
      departures.map(_.toLong).product
    }

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
          case "" | "your ticket:" | "nearby tickets:" => ()
        }
      }
      Input(fields.toSeq, mine.get, other.toSeq)
    }
  }

  val input = Input.parse(args.head)
  println(input.part2)
}
