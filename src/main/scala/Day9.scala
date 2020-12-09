import scala.collection.mutable
import scala.collection.Seq
import scala.io.Source

object Day9 extends App {
  def validNum(buf: Seq[Long], n: Long): Boolean = {
    buf.combinations(2).exists { case collection.Seq(a, b) =>
      a != b && a + b == n
    }
  }

  def invalid(buf: Seq[Long]): Option[Long] = {
    val buffer = mutable.ArrayDeque.empty[Long]
    buf.foreach { n =>
      if (buffer.length >= 25) {
        if (!validNum(buffer, n))
          return Some(n)
        buffer.dropInPlace(1)
      }
      buffer.append(n)
    }
    None
  }

  def contiguousSum(buf: Seq[Long], sum: Long): Option[Seq[Long]] = {
    val sums = mutable.ArrayBuffer.empty[Long]
    buf.indices.foreach { i =>
      0.until(i).foreach { j =>
        sums(j) += buf(i)
        if (sums(j) == sum) return Some(buf.slice(j, i + 1))
      }
      sums.append(buf(i))
    }
    None
  }

  val nums = Source.fromFile(args.head).getLines.map(_.toLong).toSeq
  val inv = invalid(nums).get
  println(inv)
  val contig = contiguousSum(nums, inv).get
  println(contig.min + contig.max)
}
