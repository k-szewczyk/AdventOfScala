package y2015

import scala.util.Try

object day1 extends Task {
  val year=2015

  override def main(): Try[Unit] = super.main()

  def part1(fileContent: List[String]): Int = {
    fileContent.flatten.map(c => if c == '(' then 1 else -1).sum
  }

  def part2(fileContent: List[String]): Int = {
    def findBasement(instructions: Iterable[Int], currentLevel: Int = 0, step: Int = 0): Int = {
      val newLevel = currentLevel + instructions.head
      if newLevel < 0 then { step + 1 }
      else findBasement(instructions.drop(1), newLevel, step + 1)
    }
    val instructions = fileContent.flatten.map(c => if c == '(' then 1 else -1)
    findBasement(instructions)
  }
}
