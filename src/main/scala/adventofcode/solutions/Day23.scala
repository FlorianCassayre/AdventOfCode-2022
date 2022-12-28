package adventofcode.solutions

import adventofcode.Definitions.*

import scala.collection.View

@main def Day23 = Day(23) { (input, part) =>

  case class Vec(i: Int, j: Int):
    infix def +(that: Vec): Vec = Vec(i + that.i, j + that.j)
    def dot(that: Vec): Int = i * that.i + j * that.j

  val initialSet = input.split(lineSeparator).map(_.map {
    case '.' => false
    case '#' => true
  }).toIndexedSeq.zipWithIndex.flatMap((row, i) => row.zipWithIndex.filter((b, _) => b).map((_, j) => Vec(i, j))).toSet

  val range = -1 to 1
  val around =
    for
      i <- range
      j <- range
      if i != 0 || j != 0
    yield Vec(i, j)

  val priorities = Seq(Vec(-1, 0), Vec(1, 0), Vec(0, -1), Vec(0, 1))
  val nextTo = priorities.map(d => d -> around.filter(_.dot(d) > 0)).toMap

  def round(state: (Set[Vec], Seq[Vec])): (Set[Vec], Seq[Vec]) =
    val (positions, currentPriorities) = state
    val proposals = positions.toSeq.map { position =>
      if around.forall(d => !positions.contains(position + d)) then
        position -> position
      else
        currentPriorities
          .find(d => nextTo(d).forall(n => !positions.contains(position + n)))
          .map(d => position -> (position + d)).getOrElse(position -> position)
    }
    val allowed = proposals.map((_, newPosition) => newPosition).groupBy(identity).view.filter((_, vs) => vs.sizeIs == 1).map((position, _) => position).toSet
    val nextPositions = proposals.map((position, newPosition) => if allowed.contains(newPosition) then newPosition else position).toSet
    val nextPriorities = currentPriorities.tail :+ currentPriorities.head
    (nextPositions, nextPriorities)

  def simulate(n: Int): Int =
    val (finalSet, _) = View.iterate((initialSet, priorities), n + 1)(round).last
    val (is, js) = (finalSet.map(_.i), finalSet.map(_.j))
    (is.max - is.min + 1) * (js.max - js.min + 1) - finalSet.size

  part(1) = simulate(10)

  def fixpoint: Int =
    def helper(state: (Set[Vec], Seq[Vec]), i: Int): Int =
      val nextState = round(state)
      if state._1 == nextState._1 then i else helper(nextState, i + 1)
    helper((initialSet, priorities), 1)

  part(2) = fixpoint

}
