package advent.of.code

import scala.io.Source

object Day1 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day1/input").getLines().toList.map(_.toInt)
    val part1Answer = lines.zip(lines.tail)
      .count { case (a, b) => a < b }
    println(part1Answer)

    val slidingWindows = lines.sliding(3).map(_.sum).toList
    val part2Answer = slidingWindows.zip(slidingWindows.tail)
      .count {case (a, b) => a < b }
    println(part2Answer)

  }
}
