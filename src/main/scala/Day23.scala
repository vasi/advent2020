import scala.collection.mutable

object Day23 extends App {
  class Elem(val value: Int) {
    var next = Option.empty[Elem]
  }
  class CircularLinkedList {
    private var head = Option.empty[Elem]
    private val byVal = mutable.Map.empty[Int, Elem]

    def insertAll(s: Iterable[Int]): Unit = {
      val elems = s.toArray.map(i => new Elem(i))
      for ((e, i) <- elems.zipWithIndex) {
        e.next = Some(elems((i + 1) % elems.length))
        byVal(e.value) = e
      }
      head = elems.headOption
    }

    def size: Int = byVal.size
    def headElem: Elem = head.get
    def elemAtValue(n: Int): Elem = byVal(n)
    def next(e: Elem): Elem = e.next.get

    def removeAfter(e: Elem): Elem = {
      val r = e.next.get
      e.next = r.next
      r.next = null
      byVal.remove(r.value)
      r
    }
    def insertAfterValue(n: Int, e: Elem): Unit = {
      val i = byVal(n)
      e.next = i.next
      i.next = Some(e)
      byVal(e.value) = e
    }
  }

  class State(val ll: CircularLinkedList) {
    private var cur = ll.headElem
    private var size = ll.size

    private def removeN(n: Int): Seq[Elem] = {
      val buf = mutable.ArrayBuffer.empty[Elem]
      for (i <- 1 to n) {
        buf.append(ll.removeAfter(cur))
      }
      buf.toSeq
    }

    private def addAfter(n: Int, es: Seq[Elem]): Unit = {
      es.reverse.foreach(e => ll.insertAfterValue(n, e))
    }

    private def destination(n: Int, removed: Seq[Elem]): Int = {
      val vs = removed.map(_.value).toSet
      var v = n
      while (true) {
         v = v - 1
        if (v <= 0)
          v += size
        if (!vs.contains(v))
          return v
      }
      sys.error("impossible")
    }

    def next(): Unit = {
      val v = cur.value
      val removed = removeN(3)
      val dest = destination(v, removed)
      addAfter(dest, removed)
      cur = ll.next(cur)
    }

    def nextN(count: Int): Unit = {
      val formatter = java.text.NumberFormat.getIntegerInstance
      for (i <- 1 to count) {
        if (i % 1_000_000 == 0)
          Console.err.println(s"Iteration ${formatter.format(i)}...")
        next()
      }
    }

    def allFromElem(e: Elem): Seq[Int] = {
      val vs = mutable.ArrayBuffer.empty[Int]
      val v = e.value
      vs.append(v)
      var c = ll.next(e)
      while (c.value != v) {
        vs.append(c.value)
        c = ll.next(c)
      }
      vs.toSeq
    }

    def print(): Unit = println(allFromElem(ll.headElem))

    def part1: String = allFromElem(ll.elemAtValue(1)).drop(1).mkString
    def part2: Long = {
      val one = ll.elemAtValue(1)
      val n1 = ll.next(one)
      val n2 = ll.next(n1)
      n1.value.toLong * n2.value.toLong
    }
  }
  object State {
    def build(labels: String, size: Option[Int] = None): State = {
      val s = size.getOrElse(labels.length)
      val is = labels.toCharArray.map(_.toString.toInt) ++ ((labels.length + 1) to s)
      val ll = new CircularLinkedList
      ll.insertAll(is)
      new State(ll)
    }
  }

  def part1: String = {
    val st = State.build(args.head)
    st.nextN(100)
    st.part1
  }

  def part2: Long = {
    val st = State.build(args.head, Some(1_000_000))
    st.nextN(10_000_000)
    st.part2
  }

  println(part2)
}
