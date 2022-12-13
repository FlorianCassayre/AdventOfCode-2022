package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day13 = Day(13) { (input, part) =>

  sealed abstract class Packet
  case class Value(value: Int) extends Packet
  case class Sequence(seq: Seq[Packet]) extends Packet

  def parse(s: String): Sequence =
    s.foldLeft((Seq(Sequence(Seq.empty)), false)) { case ((stack, lastToken), c) =>
      c match
        case '[' => (Sequence(Seq.empty) +: stack, false)
        case ']' => (Sequence(stack.tail.head.seq :+ stack.head) +: stack.tail.tail, false)
        case ',' => (stack, false)
        case _ =>
          val v = c.asDigit
          (stack.head.seq.lastOption, lastToken) match
            case (Some(Value(u)), true) => (Sequence(stack.head.seq.init :+ Value(u * 10 + v)) +: stack.tail, true)
            case _ => (Sequence(stack.head.seq :+ Value(v)) +: stack.tail, true)
    } match
      case (Seq(Sequence(Seq(head: Sequence))), _) => head

  val pairs = input.split(lineSeparator * 2).map(_.split(lineSeparator).map(parse).toSeq match
    case Seq(a, b) => (a, b)
  )

  def compare(n1: Packet, n2: Packet): Int = (n1, n2) match
    case (Value(v1), Value(v2)) => Integer.compare(v1, v2)
    case (Sequence(s1), Sequence(s2)) => s1.zip(s2).map(compare.tupled).find(_ != 0).getOrElse(Integer.compare(s1.size, s2.size))
    case (v: Value, _) => compare(Sequence(Seq(v)), n2)
    case _ => -compare(n2, n1)

  part(1) = pairs.zipWithIndex.filter((pair, _) => compare.tupled(pair) < 0).map(_._2 + 1).sum

  val dividers = Seq(2, 6).map(v => Sequence(Seq(Sequence(Seq(Value(v))))))

  part(2) = (pairs.flatMap((a, b) => Seq(a, b)) ++ dividers)
    .sortWith((a, b) => compare(a, b) < 0).zipWithIndex.filter((packet, _) => dividers.contains(packet)).map(_._2 + 1).product

}
