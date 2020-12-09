import scala.collection.mutable
import scala.io.Source

object Day9 extends App {
  def validNum(buf: collection.Seq[Long], n: Long): Boolean = {
    buf.combinations(2).exists { case collection.Seq(a, b) =>
      a != b && a + b == n
    }
  }

  val nums = Source.fromFile(args.head).getLines.map(_.toLong)
  var buffer = mutable.ArrayDeque.empty[Long]
  for (n <- nums) {
    if (buffer.length >= 25) {
      if (!validNum(buffer, n))
        println(n)
      buffer.dropInPlace(1)
    }
    buffer.append(n)
  }
}
