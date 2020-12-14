import scala.collection.mutable
import scala.io.Source

object Day14 extends App {
  class State {
    var mask = Array.empty[Char]
    var mem = mutable.Map.empty[Int, Long]
  }
  sealed trait Instruction {
    def apply(st: State): Unit
  }
  object Instruction {
    final val MaskRe = """mask = (\w+)""".r
    final val SetRe = """mem\[(\d+)] = (\d+)""".r

    def parse(line: String): Instruction = line match {
      case MaskRe(mask) =>
        Mask(mask.toCharArray.reverse)
      case SetRe(idx, value) => Set(idx.toInt, value.toLong)
    }

    def mask(v: Long, mask: Array[Char]): Long = {
      var r = v
      mask.zipWithIndex.foreach { case (i, c) =>
        if (c == '1') r = r | (1L << i)
        else if (c == '0') r = r & ~(1L << i)
      }
      r
    }
  }
  case class Mask(m: Array[Char]) extends Instruction {
    def apply(st: State): Unit = {
      st.mask = m
    }
  }
  case class Set(idx: Int, value: Long) extends Instruction {
    def apply(st: State): Unit = {
      st.mem(idx) = Instruction.mask(value, st.mask)
    }
  }

  val insts = Source.fromFile(args.head).getLines.map(Instruction.parse)
  val state = new State
  insts.foreach(_.apply(state))
  println(state.mem)
  println(state.mem.values.sum)
}
