import scala.collection.mutable
import scala.io.Source

object Day14 extends App {
  class State {
    var mask = Array.empty[Char]
    var mem = mutable.Map.empty[Long, Long]
  }
  sealed trait Instruction {
    def apply(st: State): Unit
    def apply2(st: State): Unit
  }
  object Instruction {
    final val MaskRe = """mask = (\w+)""".r
    final val SetRe = """mem\[(\d+)] = (\d+)""".r

    def parse(line: String): Instruction = line match {
      case MaskRe(mask) =>
        Mask(mask.toCharArray.reverse)
      case SetRe(idx, value) => Set(idx.toLong, value.toLong)
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
    def apply2(st: State): Unit = apply(st)
  }
  case class Set(idx: Long, value: Long) extends Instruction {
    def apply(st: State): Unit = {
      st.mem(idx) = Instruction.mask(value, st.mask)
    }
    def apply2(st: State): Unit = {
      val idxs = st.mask.zipWithIndex.foldLeft(Seq(idx)) { case(idxs, (c, i)) =>
        c match {
          case '0' => idxs
          case '1' => idxs.map(x => x | (1L << i))
          case 'X' => idxs.flatMap(x => Seq(
            x & ~(1L << i),
            x | (1L << i)))
        }
      }
      idxs.foreach(x => st.mem(x) = value)
    }
  }

  val insts = Source.fromFile(args.head).getLines.map(Instruction.parse)
  val state = new State
  insts.foreach(_.apply2(state))
//  println(state.mem)
  println(state.mem.values.sum)
}
