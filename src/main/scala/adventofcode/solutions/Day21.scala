package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day21 = Day(21) { (input, part) =>

  case class Operation(lhs: String, rhs: String, operator: (Long, Long) => Long)

  val instructions = input.split(lineSeparator).map {
    case s"$label: $right" =>
      val value = right match
        case s"$l $op $r" =>
          val operator: (Long, Long) => Long = op match
            case "+" => _ + _
            case "-" => _ - _
            case "*" => _ * _
            case "/" => _ / _
          Right(Operation(l, r, operator))
        case _ => Left(right.toLong)
      label -> value
  }.toMap

  def computeValue(key: String, cache: Map[String, Long] = Map.empty): Long =
    def compute(key: String, cache: Map[String, Long]): (Long, Map[String, Long]) =
      cache.get(key) match
        case Some(value) => (value, cache)
        case None =>
          instructions(key) match
            case Left(value) =>
              (value, cache + (key -> value))
            case Right(Operation(lhs, rhs, operator)) =>
              val (o1, cache1) = compute(lhs, cache)
              val (o2, cache2) = compute(rhs, cache1)
              val value = operator(o1, o2)
              (value, cache2)
    compute(key, cache)._1

  val root = "root"

  part(1) = computeValue(root)

  val human = "humn"

  val (v1, v2) = instructions(root) match
    case Right(Operation(lhs, rhs, _)) => (lhs, rhs)
    case _ => throw new Exception

  def bisect(start: Long, end: Long): Long =
    val mid = (start + end) / 2
    def computeFor(key: String): Long = computeValue(key, Map(human -> mid))
    val cmp = computeFor(v1).compare(computeFor(v2))
    if cmp == 0 then mid else if cmp > 0 then bisect(mid + 1, end) else bisect(start, mid - 1)

  part(2) = bisect(0, Long.MaxValue >>> 16)

}
