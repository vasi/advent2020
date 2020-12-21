import scala.io.Source

object Day21 extends App {
  case class Food(ingredients: Set[String], allergens: Set[String])
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
  case class Input(foods: Seq[Food]) {
    def allAllergens: Set[String] = foods.flatMap(_.allergens).toSet
    def possibleMapping: Map[String, Set[String]] = allAllergens.map { a =>
      a -> foods.filter(_.allergens.contains(a)).map(_.ingredients).reduce(_ & _)
    }.toMap
    def possibleIngredients: Set[String] = possibleMapping.values.reduce(_ | _)
    def part1: Int = {
      val possible = possibleIngredients
      foods.flatMap(_.ingredients).count(i => !possible.contains(i))
    }
    def actualMapping: Map[String, String] = {
      val possible = possibleMapping.view.mapValues(_.to(collection.mutable.Set)).to(collection.mutable.Map)
      val m = collection.mutable.Map.empty[String, String]
      while (possible.nonEmpty) {
        val found = possible.find(_._2.size == 1).get._1
        m(found) = possible(found).head
        possible.remove(found)
        possible.foreach { case (_, v) => v.remove(m(found)) }
      }
      m.toMap
    }
    def part2: String = actualMapping.toSeq.sortBy(_._1).map(_._2).mkString(",")
  }

  val input = Input.parse(args.head)
  println(input.part2)
}
