package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day25 = Day(25) { (input, part) =>

  val charToDigit = Map('-' -> -1, '=' -> -2)
  val digitToChar = charToDigit.map(_.swap)

  def digits = input.split(lineSeparator).map(_.map(d => charToDigit.getOrElse(d, d.asDigit)))

  val `5` = 5

  def toDecimal(snafu: Seq[Int]): Long =
    snafu.foldRight((0L, 1L)) { case (d, (sum, pow)) => (sum + d * pow, pow * `5`) }._1

  def fromDecimal(decimal: Long): String =
    def expansion(decimal: Long, acc: Seq[Int]): Seq[Int] =
      if decimal > 0 then expansion(decimal / `5`, (decimal % `5`).toInt +: acc) else acc
    val expanded = expansion(decimal, Seq.empty)
    def fold(remainingDigits: Seq[Int], acc: Seq[Int]): String =
      remainingDigits match
        case head +: tail =>
          val mod = head % `5`
          val carry = (head / `5`) + (if mod >= 3 then 1 else 0)
          val newRemaining = tail match
            case _ if carry == 0 => tail
            case tailHead +: tailTail => (tailHead + carry) +: tailTail
            case _ => Seq(carry)
          fold(newRemaining, mod +: acc)
        case _ => acc.map(d => digitToChar.getOrElse(d - `5`, d)).mkString
    fold(expanded.reverse, Seq.empty)

  part(1) = fromDecimal(digits.map(toDecimal).sum)

  part(2) = ""

}
