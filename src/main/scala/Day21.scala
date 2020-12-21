import scala.io.Source

object Day21 extends App {
  case class Food(ingredients: Set[String], allergens: Set[String])
  case class Input(foods: Seq[Food])
  object Input {
    final val LineRe = """(.*) \(contains (.*)\)""".r
    def parse(file: String): Input = {
      val foods = Source.fromFile(file).getLines.map {
        case LineRe(is, as) =>
          Food(is.split(" ").toSet, as.split(", ").toSet)
      }
      Input(foods.toSeq)
    }
  }

  val input = Input.parse(args.head)
  println(input)
}
