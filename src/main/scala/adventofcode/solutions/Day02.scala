package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day02 = Day(2) { (input, part) =>

  val games = input.split(lineSeparator).map(s => (s(0) - 'A', s(2) - 'X'))

  part(1) = games.map((a, b) => (if a == (b + 1) % 3 then 0 else if a == b then 3 else 6) + b + 1).sum

  part(2) = games.map((a, b) =>
    b match
      case 0 => (a + 2) % 3
      case 1 => 3 + a
      case 2 => 6 + (a + 1) % 3
  ).map(_ + 1).sum

}
