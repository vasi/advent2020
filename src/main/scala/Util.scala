import scala.collection.mutable.ArrayBuffer

object Util {
  def splitLines(it: Iterator[String]): Array[Array[String]] = {
    val buf = new ArrayBuffer[Array[String]]
    var rem = it
    while (rem.nonEmpty) {
      val (pre, post) = it.span(!_.isEmpty)
      buf.append(pre.toArray)
      rem = post.drop(1)
    }
    buf.toArray
  }
  def posMod(n: Int, d: Int): Int = {
    val m = n % d
    if (m < 0) m + d
    else m
  }
}
