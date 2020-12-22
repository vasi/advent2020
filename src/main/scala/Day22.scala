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
    def iWinRound(me: Deck, crab: Deck, code: String): Boolean = {
      me.head > crab.head
    }

    def gameCode(me: Deck, crab: Deck): String = Seq(me, crab).map(_.map(_.toString).mkString(",")).mkString("/")

    def play(me: Deck, crab: Deck): Unit = {
      val seen = mutable.Set.empty[String]
      while (me.nonEmpty && crab.nonEmpty) {
        val code = gameCode(me, crab)
        if (!seen.add(code)) {
          me.appendAll(crab)
          crab.clear()
          return
        }
        if (iWinRound(me, crab, code))
          me.append(me.removeHead()).append(crab.removeHead())
        else
          crab.append(crab.removeHead()).append(me.removeHead())
      }
    }

    def iWinGame(me: Deck, crab: Deck): Boolean = {
      play(me, crab)
      me.nonEmpty
    }
  }

  class RecursiveCombat extends Combat {
    val iWinCache = mutable.Map.empty[String, Boolean]

    override def iWinRound(me: Deck, crab: Deck, code: String): Boolean = {
      iWinCache.getOrElseUpdate(code, {
        if (me.size > me.head && crab.size > crab.head) {
          val nextMe = me.slice(1, 1 + me.head)
          val nextCrab = crab.slice(1, 1 + crab.head)
          iWinGame(nextMe, nextCrab)
        }
        else me.head > crab.head
      })
    }
  }

  def score(me: Deck, crab: Deck): Int = {
    val d = if (me.nonEmpty) me else crab
      d.reverse.zipWithIndex.map { case (c, i) => c * (i+1) }.sum
  }

  val (me, crab) = parse(args.head)
  val combat = new RecursiveCombat
  combat.play(me, crab)
  println(score(me, crab))
}
