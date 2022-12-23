package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day20 = Day(20) { (input, part) =>

  def mod(a: Long, b: Long): Long = ((a % b) + b) % b

  val numbers = input.split(lineSeparator).map(_.toLong).toVector.toIndexedSeq

  def compute(list: IndexedSeq[Long], times: Int): Long =
    val listWithIndices = list.zipWithIndex
    val size = listWithIndices.size
    val result = (0 until times).foldLeft(listWithIndices) { (currentNumbers, _) =>
      listWithIndices.foldLeft(currentNumbers) { case (acc, (e, i)) =>
        val index = acc.indexOf((e, i))
        val (left, right) = acc.splitAt(index)
        val front = right.tail ++ left
        val k = mod(e, size - 1)
        val (l, r) = front.splitAt(k.toInt)
        (l :+ (e, i)) ++ r
      }
    }.map(_._1)
    val (left, right) = result.splitAt(result.indexOf(0))
    val concat = right ++ left
    val grove = (1 to 3).map(_ * 1000)
    grove.map(i => concat(i % concat.size)).sum

  part(1) = compute(numbers, 1)

  val decryptionKey = 811589153L

  part(2) = compute(numbers.map(_ * decryptionKey), 10)

}
