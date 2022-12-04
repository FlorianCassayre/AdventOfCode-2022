package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day04 = Day(4) { (input, part) =>

  val interval = input.split(lineSeparator).map {
    case s"$a-$b,$c-$d" => (a.toInt to b.toInt, c.toInt to d.toInt)
  }

  part(1) = interval.count((a, b) => a.intersect(b).size == Math.min(a.size, b.size))

  part(2) = interval.count(_.intersect(_).nonEmpty)

}
