package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day08 = Day(8) { (input, part) =>

  val grid = input.split(lineSeparator)

  part(1) = grid.indices.map(i => grid(i).indices.count { j =>
    val t = grid(i)(j)
    grid.indices.take(i).forall(i1 => grid(i1)(j) < t) ||
      grid.indices.drop(i + 1).forall(i1 => grid(i1)(j) < t) ||
      grid(i).indices.take(j).forall(j1 => grid(i)(j1) < t) ||
      grid(i).indices.drop(j + 1).forall(j1 => grid(i)(j1) < t)
  }).sum

  def d(s: Seq[Char]): Int =
    val index = s.tail.indexWhere(_ >= s.head)
    if index >= 0 then index + 1 else s.length - 1

  part(2) = grid.indices.flatMap(i => grid(i).indices.map { j =>
    d(grid.indices.take(i + 1).reverse.map(i1 => grid(i1)(j))) *
      d(grid.indices.drop(i).map(i1 => grid(i1)(j))) *
      d(grid(i).indices.take(j + 1).reverse.map(j1 => grid(i)(j1))) *
      d(grid(i).indices.drop(j).map(j1 => grid(i)(j1)))
  }).max

}
