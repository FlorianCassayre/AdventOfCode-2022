package adventofcode.solutions

import adventofcode.Definitions.*

import scala.collection.View

@main def Day12 = Day(12) { (input, part) =>

  case class Vec(i: Int, j: Int):
    infix def +(that: Vec): Vec = Vec(i + that.i, j + that.j)

  val grid = input.split(lineSeparator)

  def find(c: Char): View[Vec] = grid.indices.view.flatMap(i => grid(i).indices.view.flatMap(j => if grid(i)(j) == c then Some(Vec(i, j)) else None))

  val (start, end) = (find('S').head, find('E').head)
  val (lowest, highest) = ('a', 'z')

  def canCross(from: Vec, to: Vec): Boolean =
    val valueFrom = grid(from.i)(from.j)
    from == start || (if to == end then valueFrom == highest else valueFrom + 1 >= grid(to.i)(to.j))

  val directions = Seq(-1, 1).flatMap(i => Seq(Vec(i, 0), Vec(0, i)))

  def dfs(current: Set[Vec], visited: Set[Vec] = Set.empty, i: Int = 0): Int =
    if current.contains(end) then
      i
    else
      val newVisited = visited ++ current
      val next = current.flatMap { from =>
        directions
          .map(from + _)
          .filter(to => !newVisited.contains(to) && grid.indices.contains(to.i) && grid(to.i).indices.contains(to.j) && canCross(from, to))
      }
      dfs(next, newVisited, i + 1)

  part(1) = dfs(Set(start))

  part(2) = dfs(find(lowest).toSet)

}
