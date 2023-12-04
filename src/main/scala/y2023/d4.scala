package y2023

import scala.annotation.tailrec

object day4 extends Task {
  val year = 2023

  @main
  override def main(): Unit = super.main()

  case object Card {
    private val cardNrPattern = "Card\\s+(?<CardNumber>\\d+?):".r
    def parseFromLine(line: String): Card = {
      val cardNumber = Integer.parseInt(cardNrPattern.findFirstMatchIn(line).map(_.group("CardNumber")).get)
      val numbersInGame = line.split(":").last.split("\\|")
      val winningNumbers = numbersInGame.head.trim.split(" ").filterNot(_.isBlank).map(Integer.parseInt)
      val numbersYouHave = numbersInGame.last.trim.split(" ").filterNot(_.isBlank).map(Integer.parseInt)
      Card(cardNr = cardNumber, winningNumbers = winningNumbers, numbersYouHave = numbersYouHave)
    }
  }
  case class Card(cardNr: Int, winningNumbers: Seq[Int], numbersYouHave: Seq[Int]) {

    def calculateScore(winningNumbersFiltered: List[Int]): Int = {
      @tailrec
      def double(wnf: List[Int], acc: Int): Int = {
        if wnf.isEmpty then acc
        else double(wnf.tail, acc * 2)

      }
      if winningNumbersFiltered.length == 1 then 1
      else if winningNumbersFiltered.isEmpty then 0
      else double(winningNumbersFiltered.tail, 1)
    }
    def calculateCardResultPart1(): Int = {
      val matchedNumbers = numbersYouHave.filter(winningNumbers.contains(_)).toList
      calculateScore(matchedNumbers)
    }

    def calculateCardResultPart2(): Seq[Int] = {
      val matchedNumbers = numbersYouHave.filter(winningNumbers.contains(_))
      (1 to matchedNumbers.length).map(_ + cardNr)
    }
  }

  override def part1(value: Iterator[String]): Any = {
    value
      .map(Card.parseFromLine)
      .map(_.calculateCardResultPart1())
      .sum

  }

  def traverseWinningCards(cards: Iterator[Card]): Int = {
    val emptyList: List[Int] = List.empty
    cards.foldRight(emptyList){ case (card, acc) =>
      val r = card.calculateCardResultPart2()
      val sum = acc.take(r.length).sum + 1
      sum :: acc
    }.sum
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }
  override def part2(value: Iterator[String]): Any =  {
      traverseWinningCards(value.map(Card.parseFromLine))
  }
}
