package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day14 = Day(14) { (input, part) =>

  case class Vec(x: Int, y: Int):
    def +(that: Vec): Vec = Vec(x + that.x, y + that.y)

  val paths = input.split(lineSeparator).map(_.split(" -> ").map {
    case s"$a,$b" => Vec(a.toInt, b.toInt)
  })

  val maxY = paths.flatten.map(_.y).max

  val map = paths.flatMap { path =>
    path.zip(path.tail).flatMap { case (a, b) =>
      def range(u: Int, v: Int): Range = Math.min(u, v) to Math.max(u, v)
      if a.x == b.x then range(a.y, b.y).map(y => Vec(a.x, y)) else range(a.x, b.x).map(x => Vec(x, a.y))
    }
  }

  val source = Vec(500, 0)

  def simulate(sand: Set[Vec], i: Int): Int =
    def isBlocked(position: Vec): Boolean = map.contains(position) || sand.contains(position)
    def pour(grain: Vec): Option[Vec] =
      val attempts = Seq(Vec(0, 1), Vec(-1, 1), Vec(1, 1)).map(_ + grain)
      attempts.find(p => !isBlocked(p)) match
        case Some(next) =>
          if next.y > maxY then None else pour(next)
        case None => Some(grain)
    pour(source) match
      case Some(settled) => simulate(sand + settled, i + 1)
      case None => i

  part(1) = simulate(Set.empty, 0)

  part(2) = (0 until maxY + 2).foldLeft((Set(source), 0)) { case ((sand, count), y) =>
    val next = sand.flatMap(p => Seq(p.copy(x = p.x - 1), p, p.copy(x = p.x + 1)).map(_.copy(y = y + 1))).filter(p => !map.contains(p))
    (next, count + sand.size)
  }._2

}
