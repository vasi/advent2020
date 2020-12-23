import scala.collection.mutable

object Day23 extends App {
  type State = mutable.ArrayDeque[Int]

  def nextRound(st: State): Unit = {
    val cur = st.removeHead()
    val move = st.take(3)
    st.remove(0, 3)

    val destIdx = st.indices.minBy {i =>
      (cur + 10 - st(i)) % 10
    }
    st.insertAll(destIdx + 1, move)
    st.append(cur)
  }

  def part1(st: State): String = {
    val i = st.indexOf(1)
    (st.drop(i + 1) ++ st.take(i)).mkString
  }

  val init = args.head
  val st = init.toCharArray.map(_.toString.toInt).to(mutable.ArrayDeque)
  for (i <- 1 to 100)
    nextRound(st)
  println(part1(st))
}
