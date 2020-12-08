import scala.io.Source
import scala.collection.mutable

object Day8 extends App {
  case class State(pc: Int = 0, acc: Int = 0)

  trait Instruction {
    def process(st: State): State
  }

  object Instruction {
    final val InstRe = """(nop|acc|jmp) ([+-]\d+)""".r

    def parse(file: String): Seq[Instruction] = Source.fromFile(file).getLines().map { line =>
      val inst = line match {
        case InstRe(name, arg) => name match {
          case "nop" => Nop(arg.toInt)
          case "acc" => Acc(arg.toInt)
          case "jmp" => Jmp(arg.toInt)
        }
      }
      inst.asInstanceOf[Instruction]
    }.toSeq
  }

  case class Nop(arg: Int) extends Instruction {
    def process(st: State): State = st.copy(pc = st.pc + 1)
  }
  case class Jmp(offset: Int) extends Instruction {
    def process(st: State): State = st.copy(pc = st.pc + offset)
  }
  case class Acc(arg: Int) extends Instruction {
    def process(st: State): State = st.copy(pc = st.pc + 1, acc = st.acc + arg)
  }

  def run(insts: Seq[Instruction]): State = {
    var st = State()
    val seen = mutable.Set.empty[Int]
    while (st.pc < insts.length && !seen.contains(st.pc)) {
      seen.add(st.pc)
      st = insts(st.pc).process(st)
    }
    st
  }

  val insts = Instruction.parse(args.head)
//  println(run(insts).acc)

  def findBadInst(insts: Seq[Instruction]): (Int, State) = {
    val newInsts = insts.indices.flatMap { idx =>
      val change = insts(idx) match {
        case Acc(_) => None
        case Nop(arg) => Some(Jmp(arg))
        case Jmp(arg) => Some(Nop(arg))
      }
      change.map(i => idx -> insts.updated(idx, i))
    }
    val results = newInsts.map { case (idx, i2s) => idx -> run(i2s) }
    results.find { case (_, st) => st.pc >= insts.length }.get
  }

  println(findBadInst(insts))
}
