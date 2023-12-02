package y2023

import scala.None

object day1 extends Task {
  val year = 2023

  override def main(): Unit = super.main()

  def part1(fileContent: List[String]): Int = {
    fileContent.map { line =>
      extractDigits(line)
    }.sum
  }

  def part2(fileContent: List[String]): Int = {
    val digitWords: Seq[(String, String)] = Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
      .zipWithIndex.map((numStr, idx) => numStr -> (idx + 1).toString)
    val letters = fileContent.map { line =>
      val digitsWithIndex = digitWords.flatMap { (numberWord, number) =>
        val firstDigits = line.replaceAll(numberWord, number)
          .zipWithIndex
          .filter(_._1.isDigit)
          .sortBy(_._2)
        val lastDigits = line.replaceAll(numberWord, number)
          .reverse
          .zipWithIndex
          .filter(_._1.isDigit)
          .sortBy(_._2)
        if firstDigits.isEmpty then None
        else Some(firstDigits.head, lastDigits.head)
      }
      Integer.parseInt(Seq(digitsWithIndex.minBy(_._1._2)._1._1, digitsWithIndex.minBy(_._2._2)._2._1).mkString(""))
      }
      letters.sum
  }

  private def extractDigits(line: String) = {
    val firstDigit = line.find(_.isDigit).get
    val lastDigit = line.findLast(_.isDigit).get
    Integer.parseInt(Seq(firstDigit, lastDigit).mkString(""))
  }
}
