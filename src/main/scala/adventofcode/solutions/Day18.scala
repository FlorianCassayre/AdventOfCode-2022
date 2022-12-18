package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day18 = Day(18) { (input, part) =>

  case class Vec(x: Int, y: Int, z: Int):
    def manhattan(that: Vec): Int = (x - that.x).abs + (y - that.y).abs + (z - that.z).abs
    def adjacent(that: Vec): Boolean = manhattan(that) == 1
    def +(that: Vec): Vec = Vec(x + that.x, y + that.y, z + that.z)

  val data = input.split(lineSeparator).map {
    case s"$a,$b,$c" => Vec(a.toInt, b.toInt, c.toInt)
  }.toSet

  val range = -1 to 1
  val zero = Vec(0, 0, 0)
  val sides =
    for
      a <- range
      b <- range
      c <- range
      v = Vec(a, b, c)
      if v.adjacent(zero)
    yield v

  part(1) = (
    for
      v <- data.toSeq
      s <- sides
      if !data.contains(v + s)
    yield 1
    ).sum

  val (maxX, maxY, maxZ) = (data.map(_.x).max + 1, data.map(_.y).max + 1, data.map(_.z).max + 1)

  def bfs(current: Set[Vec], visited: Set[Vec]): Set[Vec] =
    if current.nonEmpty then
      val nextVisited = visited ++ current
      val nextCurrent = current.flatMap(c => sides.map(_ + c)).diff(nextVisited).diff(data).filter(v => v.x >= 0 && v.x <= maxX && v.y >= 0 && v.y <= maxY && v.z >= 0 && v.z <= maxZ)
      bfs(nextCurrent, nextVisited)
    else
      visited

  val initialOutside =
    for
      x <- 0 to maxX
      y <- 0 to maxY
      z <- 0 to maxZ
      if x == 0 || x == maxX || y == 0 || y == maxY || z == 0 || z == maxZ
    yield Vec(x, y, z)

  val outside = bfs(initialOutside.toSet, Set.empty)

  part(2) = (
    for
      v <- data.toSeq
      s <- sides
      if outside.contains(v + s)
    yield 1
    ).sum

}
