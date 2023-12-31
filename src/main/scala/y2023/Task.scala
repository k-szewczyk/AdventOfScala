package y2023

import scala.io.Source
import scala.util.{Failure, Success, Try, Using}

trait Task {
  val year: Int
  def main(): Unit = {
    val day = getClass.getName.split('.').last.replace("$", "")
    Using(Source.fromResource(s"${year}/${day}p1.txt")) { file =>
      Console.println(part1(file.getLines()).toString)
    } match
      case Failure(exception) => print(exception.toString)
      case Success(value) => value

    Using(Source.fromResource(s"${year}/${day}p2.txt")) { file =>
      Console.println(part2(file.getLines()).toString)
    } match
      case Failure(exception) => print(exception.toString)
      case Success(value) => value
  }
  def part1(value: Iterator[String]): Any
  def part2(value: Iterator[String]): Any
}
