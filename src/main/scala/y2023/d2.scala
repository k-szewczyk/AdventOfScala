package y2023



sealed trait Color

case object Red extends Color
case object Blue extends Color
case object Green extends Color

case class Subset(rolls: Seq[Roll])
case class Roll(color: Color, count: Int)
case class Game(number: Int, subsets: Seq[Subset])

object day2 extends Task {
  val year = 2023

  @main
  override def main(): Unit = super.main()
  val part1EdgeCase: Map[Color, Int] = Map(Red -> 12, Green -> 13, Blue -> 14)

  val gameRegex = ("Game (?<GameNr>\\d+)").r
  def getRoll(roll: String) =
    val rollPair = roll.trim.split(" ")
    val count = Integer.parseInt(rollPair.head)
    val color = rollPair.last.toLowerCase match
      case "red" => Red
      case "green" => Green
      case "blue" => Blue
    Roll(color, count)
  def parseInput(line: String): Game = {
    val gameNr = gameRegex.findAllMatchIn(line)
      .map(x => Integer.parseInt(x.group("GameNr")))
      .toList
      .head

    val subsets = line.split(":")
      .last
      .split(";")
      .map { subset =>
        subset
          .split(", ")
          .map(getRoll)
          .toSeq
      }
      .map(Subset.apply)
    Game(gameNr, subsets)
  }
  def getMaxColorsForSubset(game: Game): Seq[Map[Color, Int]] = {
    game.subsets
      .map(subset =>
        subset.rolls
          .groupBy(_.color)
          .map(x => x._1 -> x._2.map(_.count).sum)
      )
  }

  def solvep1(game: Game): Boolean = {
    !getMaxColorsForSubset(game)
      .flatMap(x => x.map(y => if part1EdgeCase(y._1) >= y._2 then true else false))
      .contains(false)
  }

  def solvep2(game:Game): Int = {
    getMaxColorsForSubset(game)
      .flatten
      .groupBy(_._1)
      .map { case (color, value) => color -> value.map(_._2) }
      .map { case (color, value) => color -> value.max }
      .values.product
  }

  def part1(fileContent: List[String]): Int = {
    fileContent
      .map(parseInput)
      .filter(solvep1)
      .map(_.number)
      .sum
  }

  def part2(fileContent: List[String]): Int = {
    fileContent
      .map(parseInput)
      .map(solvep2)
      .sum
  }
}
