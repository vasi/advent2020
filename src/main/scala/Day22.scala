import scala.collection.mutable
import scala.io.Source

object Day22 extends App {
  type Deck = mutable.ArrayDeque[Int]

  final val PlayerRe = """Player (\d+):""".r
  final val CardRe = """(\d+)""".r
  def parse(file: String): Seq[Deck] = {
    val decks = (1 to 2).map(_ => mutable.ArrayDeque.empty[Int])
    var deck = -1
    Source.fromFile(file).getLines.foreach {
      case "" => ()
      case PlayerRe(p) => deck = p.toInt - 1
      case CardRe(c) => decks(deck).append(c.toInt)
    }
    decks
  }

  def play(decks: Seq[Deck]): Unit = {
    while (!decks.exists(_.isEmpty)) {
      val w = decks.indices.maxBy(i => decks(i).head)
      decks(w).append(decks(w).removeHead())
      decks.indices.filterNot(_ == w).foreach(i => decks(w).append(decks(i).removeHead()))
    }
  }
  def part1(decks: Seq[Deck]): Int = {
    play(decks)
    decks.find(_.nonEmpty).get.reverse.zipWithIndex.map { case (c, i) => c * (i+1) }.sum
  }

  val decks = parse(args.head)
  println(part1(decks))
}
