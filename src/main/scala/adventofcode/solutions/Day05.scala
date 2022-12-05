package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day05 = Day(5) { (input, part) =>

  val (buckets, instructions) = input.split(lineSeparator * 2).toSeq match
    case Seq(first, second) =>
      val lines = first.split(lineSeparator)
      val range = 1 until lines.last.size by 4
      val buckets = lines.init.map(s => range.map(i => if s.indices.contains(i) && s(i).isLetter then Some(s(i)) else None)).toIndexedSeq.transpose.map(_.flatten)
      val instructions = second.split(lineSeparator).map {
        case s"move $a from $b to $c" => (a.toInt, b.toInt - 1, c.toInt - 1)
      }
      (buckets, instructions)

  def execute(tpe: Boolean): String =
    instructions.foldLeft(buckets) { case (acc, (amount, from, to)) =>
      val (moved, fromRemaining) = acc(from).splitAt(amount)
      acc.updated(from, fromRemaining).updated(to, (if tpe then moved else moved.reverse) ++ acc(to))
    }.map(_.head).mkString

  part(1) = execute(false)

  part(2) = execute(true)

}
