package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day01 = Day(1) { (input, part) =>

  val sums = input.split(lineSeparator * 2).map(_.split(lineSeparator).map(_.toInt).sum)

  part(1) = sums.max

  part(2) = sums.sorted.takeRight(3).sum

}
