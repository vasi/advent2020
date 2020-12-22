import scala.collection.mutable
import scala.io.Source

object Day22 extends App {
  type Deck = mutable.ArrayDeque[Int]

  final val PlayerRe = """Player (\d+):""".r
  final val CardRe = """(\d+)""".r
  def parse(file: String): (Deck, Deck) = {
    val decks = (1 to 2).map(_ => mutable.ArrayDeque.empty[Int])
    var deck = -1
    Source.fromFile(file).getLines.foreach {
      case "" => ()
      case PlayerRe(p) => deck = p.toInt - 1
      case CardRe(c) => decks(deck).append(c.toInt)
    }
    (decks(0), decks(1))
  }

  class Combat {
    def iWinRound(me: Deck, crab: Deck): Boolean = me.head > crab.head

    def play(me: Deck, crab: Deck): Unit = {
      while (me.nonEmpty && crab.nonEmpty) {
        if (iWinRound(me, crab))
          me.append(me.removeHead()).append(crab.removeHead())
        else
          crab.append(crab.removeHead()).append(me.removeHead())
      }
    }
  }

  def score(me: Deck, crab: Deck): Int = {
    val d = if (me.nonEmpty) me else crab
      d.reverse.zipWithIndex.map { case (c, i) => c * (i+1) }.sum
  }

  val (me, crab) = parse(args.head)
  val combat = new Combat
  combat.play(me, crab)
  println(score(me, crab))
}
