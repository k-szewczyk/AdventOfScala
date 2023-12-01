import scala.io.Source
import scala.util.Using

trait Task {
  val year: Int
  def main() = {
    val day = getClass.getName.split('.').last.replace("$", "")
    Using(Source.fromResource(s"${year}/${day}p1.txt")) { file =>
      Console.println(part1(file.getLines().toList).toString)
    }
    Using(Source.fromResource(s"${year}/${day}p2.txt")) { file =>
      Console.println(part2(file.getLines().toList).toString)
    }
  }
  def part1(value: List[String]): Any
  def part2(value: List[String]): Any
}
