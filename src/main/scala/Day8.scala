import scala.io.Source
import scala.collection.mutable

object Day8 extends App {
  case class State(pc: Int = 0, acc: Int = 0)

  trait Instruction {
    def process(st: State): State
  }

  object Instruction {
    final val InstRe = """(nop|acc|jmp) ([+-]\d+)""".r

    def parse(file: String): Array[Instruction] = Source.fromFile(file).getLines().map { line =>
      val inst = line match {
        case InstRe(name, arg) => name match {
          case "nop" => Nop()
          case "acc" => Acc(arg.toInt)
          case "jmp" => Jmp(arg.toInt)
        }
      }
      inst.asInstanceOf[Instruction]
    }.toArray
  }

  case class Nop() extends Instruction {
    def process(st: State): State = st.copy(pc = st.pc + 1)
  }
  case class Jmp(offset: Int) extends Instruction {
    def process(st: State): State = st.copy(pc = st.pc + offset)
  }
  case class Acc(arg: Int) extends Instruction {
    def process(st: State): State = st.copy(pc = st.pc + 1, acc = st.acc + arg)
  }

  def runUntilLoop(insts: Array[Instruction]): State = {
    var st = State()
    val seen = mutable.Set.empty[Int]
    while (!seen.contains(st.pc)) {
      seen.add(st.pc)
      st = insts(st.pc).process(st)
    }
    st
  }

  val insts = Instruction.parse(args.head)
  println(runUntilLoop(insts).acc)
}
