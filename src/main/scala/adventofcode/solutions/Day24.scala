package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day24 = Day(24) { (input, part) =>

  case class Vec(i: Int, j: Int):
    infix def +(that: Vec): Vec = Vec(i + that.i, j + that.j)

  val map =
    val lines = input.split(lineSeparator)
    lines.tail.init.map(_.tail.init.map {
      case '.' => None
      case '^' => Some(Vec(-1, 0))
      case 'v' => Some(Vec(1, 0))
      case '<' => Some(Vec(0, -1))
      case '>' => Some(Vec(0, 1))
    }).toIndexedSeq

  val (width, height) = (map.head.size, map.size)
  val (start, end) = (Vec(-1, 0), Vec(height, width - 1))

  val blizzards = map.zipWithIndex.flatMap((row, i) => row.zipWithIndex.flatMap((option, j) => option.map(Vec(i, j) -> _)))

  val directions =
    val range = -1 to 1
    for
      i <- range
      j <- range
      if i.abs + j.abs <= 1
    yield Vec(i, j)

  def bfs(condition: (Vec, Int), positions: Set[(Vec, Int)] = Set((start, 0)), currentBlizzards: Seq[(Vec, Vec)] = blizzards, i: Int = 0): Int =
    if positions.contains(condition) then
      i
    else
      val nextBlizzards = currentBlizzards.map { (position, direction) =>
        val nextPosition = position + direction
        Vec((nextPosition.i + height) % height, (nextPosition.j + width) % width) -> direction
      }
      val nextBlizzardsSet = nextBlizzards.map((position, _) => position).toSet
      val nextPositions = positions
        .flatMap((position, reached) => directions.map(d => (position + d, reached)))
        .filter((position, _) => position == start || position == end || (map.indices.contains(position.i) && map.head.indices.contains(position.j)))
        .filter((position, _) => !nextBlizzardsSet.contains(position))
        .map((position, reached) =>
          (position, reached + (if reached % 2 == 0 && position == end || reached % 2 == 1 && position == start then 1 else 0))
        )
      bfs(condition, nextPositions, nextBlizzards, i + 1)

  part(1) = bfs((end, 1))

  part(2) = bfs((end, 3))

}
