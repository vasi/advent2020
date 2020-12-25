object Day25 extends App {
  final val Modulus = 20201227L
  final val Subject = 7

  def transform(subj: Long, loops: Int): Long = {
    var a = 1L
    for (i <- 1 to loops) {
      a = (a * subj) % Modulus
    }
    a
  }

  def findLoops(subj: Long, result: Long): Int = {
    var a = 1L
    var i = 0
    while (true) {
      if (a == result) return i
      a = (a * subj) % Modulus
      i += 1
    }
    sys.error("can't get here")
  }

  def part1(pubkeys: Seq[Long]): Long = {
    val loops = findLoops(Subject, pubkeys.head)
    transform(pubkeys.last, loops)
  }

  val pubkeys = args.map(_.toLong)
  println(part1(pubkeys))
}
