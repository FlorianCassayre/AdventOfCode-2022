package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day06 = Day(6) { (input, part) =>

  def solve(n: Int): Int = input.sliding(n).zipWithIndex.find((s, _) => s.toSet.size == n).head._2 + n

  part(1) = solve(4)

  part(2) = solve(14)

}
