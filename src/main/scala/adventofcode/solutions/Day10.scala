package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day10 = Day(10) { (input, part) =>

  val result = input.split(lineSeparator).foldLeft(Seq(1)) { (history, op) =>
    val value = history.head
    op match
      case "noop" => value +: history
      case s"addx $add" => (value + add.toInt) +: value +: history
  }.reverse

  val (width, height) = (40, 6)
  val range = (0 until height).map(_ * width + width / 2)

  part(1) = range.map(i => i * result(i - 1)).sum

  val pixels = result.zipWithIndex.flatMap((v, i) => if (v - (i % width)).abs <= 1 then Some(i) else None).toSet

  part(2) = (0 until height).map(i => (0 until width).map(j => if pixels.contains(i * width + j) then "#" else ".").mkString).mkString(lineSeparator)

}
