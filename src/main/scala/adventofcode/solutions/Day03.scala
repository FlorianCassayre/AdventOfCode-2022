package adventofcode.solutions

import adventofcode.Definitions.*
import scala.util.chaining.*

@main def Day03 = Day(3) { (input, part) =>

  val lines = input.split(lineSeparator)
  def priority(c: Char): Int = if c.isLower then c - 'a' + 1 else c - 'A' + 27

  part(1) = lines.map(l => l.splitAt(l.size / 2).pipe((a, b) => a.toSet.intersect(b.toSet).head)).map(priority).sum

  part(2) = lines.grouped(3).map(s => s.map(_.toSet).reduce(_.intersect(_)).head).map(priority).sum

}
