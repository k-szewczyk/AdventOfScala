package y2023

object day3 extends Task {
  val year = 2023

  override def main(): Unit = super.main()

  case class PartNumber(line: Int, startPos: Int, number: Int) {

    def getNeighbourhood(lines: Iterator[String]) = lines.toVector.slice(line - 1, line + 2)
      .map(_.slice(startPos - 1, startPos + number.toString.length + 1))

    def hasSpecialSignAdjacent(lines: Iterator[String]): Boolean = {
      getNeighbourhood(lines)
        .map(_.replaceAll("\\d+", "").replace(".", "").replaceAll("\\s", ""))
        .exists(_.nonEmpty)
    }

    def isConnectedToGear(lines: Iterator[String]): Boolean = {
      getNeighbourhood(lines)
        .exists(_.contains("*"))
    }
  }

  case class Gear(line: Int, pos: Int) {
    def getAdjancedNumbers(numbers: Seq[PartNumber]) = {
      numbers.filter { part =>
        (line - 1 <= part.line && part.line <= line + 1) && (pos >= part.startPos - 1 && pos <= (part.startPos + part.number.toString.length))
      }
    }
  }

  def getNumbers(lines: Iterator[String]): Seq[PartNumber] = {
    lines
      .toVector
      .zipWithIndex
      .flatMap { case (str, i) =>
        var mem = 0
        str.split("\\D")
          .filter("\\d+".r.matches)
          .map { x =>
            val index = str.indexOf(x, mem)
            mem = index + x.length
            PartNumber(i, index, Integer.parseInt(x))

          }
      }
  }

  def getSpecialCharctersPositions(lines: Iterator[String]): Seq[Gear] = {
    lines
      .toVector
      .zipWithIndex
      .flatMap { case (str, i) =>
        var mem = 0
        str.flatMap {
          case x if x == '*' =>
            val index = str.indexOf(x, mem)
            mem = index + 1
            Some(Gear(i, index))
          case _ => None
        }
      }
  }


  override def part1(value: Iterator[String]): Any = {
    getNumbers(value)
      .filter(_.hasSpecialSignAdjacent(value))
      .map(_.number)
      .sum
  }
  override def part2(value: Iterator[String]): Any =  {
    val numbers = getNumbers(value)
    val gears = getSpecialCharctersPositions(value)
    gears.map(_.getAdjancedNumbers(numbers))
      .filter(_.length >= 2)
      .map(x => x.map(_.number).product)
      .sum

//      .map(_.getAdjancedNumbers(numbers))
//      .filter(_.length >= 2)


  }
}
