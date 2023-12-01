package y2015

import scala.annotation.tailrec
import scala.util.Try

object day2 extends Task {
  val year = 2023

  override def main(): Try[Unit] = super.main()

  def part1(fileContent: List[String]): Int = {
    @tailrec
    def getPairs(seq: Iterable[Int], accumulator: List[List[Int]]): List[List[Int]]= {
      seq.headOption match{
        case Some(value) =>
          getPairs(seq.drop(1), accumulator ++ seq.drop(1).map(List(_, value)))
        case None => accumulator
      }
    }
    fileContent.map({ x => {
      val wallPairs = getPairs(x.split("x").map(_.toInt), List.empty)
      val sidesAreas = wallPairs.map(_.product)
      val smallestSide = sidesAreas.min
      sidesAreas.sum * 2 + smallestSide
    }}).sum
  }

  def part2(fileContent: List[String]): Int = {
    0
  }
}
