package adventofcode.solutions

import adventofcode.Definitions.*

import scala.collection.View

@main def Day11 = Day(11) { (input, part) =>

  case class Monkey(id: Int, items: Seq[Long], operator: (Long, Long) => Long, opRhs: Option[Long], divisible: Long, trueTo: Int, falseTo: Int)

  val monkeys = input.split(lineSeparator * 2).map(_.split(lineSeparator).toSeq match
    case Seq(
    s"Monkey $id:",
    s"  Starting items: $items",
    s"  Operation: new = old $operator $opRhs",
    s"  Test: divisible by $divisible",
    s"    If true: throw to monkey $trueTo",
    s"    If false: throw to monkey $falseTo"
    ) =>
      val op: (Long, Long) => Long = operator match
        case "*" => _ * _
        case "+" => _ + _
      val rhs = opRhs match
        case "old" => None
        case _ => Some(opRhs.toLong)
      Monkey(id.toInt, items.split(", ").map(_.toLong), op, rhs, divisible.toLong, trueTo.toInt, falseTo.toInt)
  ).toIndexedSeq

  val group = monkeys.map(_.divisible).product

  def round(monkeys: IndexedSeq[Monkey], modulo: Boolean): (IndexedSeq[Monkey], IndexedSeq[Int]) =
    monkeys.indices.foldLeft((monkeys, IndexedSeq.empty[Int])) { case ((state, counts), id) =>
      val monkey = state(id)
      (monkey.items.foldLeft(state.updated(monkey.id, monkey.copy(items = Seq.empty))) { (state, item) =>
        val itemBefore = monkey.operator(item, monkey.opRhs.getOrElse(item))
        val itemAfter = if modulo then itemBefore % group else itemBefore / 3
        val to = if itemAfter % monkey.divisible == 0 then monkey.trueTo else monkey.falseTo
        val toMonkey = state(to)
        state.updated(to, toMonkey.copy(items = toMonkey.items :+ itemAfter))
      }, counts :+ monkey.items.size)
    }

  def compute(steps: Int, modulo: Boolean): Long =
    View.iterate((monkeys, monkeys.map(_ => 0)), steps + 1) { (monkeys, counts) =>
      val (newMonkeys, newCounts) = round(monkeys, modulo)
      (newMonkeys, counts.zip(newCounts).map(_ + _))
    }.last._2.sorted.takeRight(2).map(_.toLong).product

  part(1) = compute(20, false)

  part(2) = compute(10000, true)

}
