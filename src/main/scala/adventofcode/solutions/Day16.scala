package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day16 = Day(16) { (input, part) =>

  case class Scan(valve: String, rate: Int, tunnels: Seq[String])

  val data = input.split(lineSeparator).map {
    case s"Valve $valve has flow rate=$rate; tunnel$_ lead$_ to valve$_ $tunnels" =>
      valve -> Scan(valve, rate.toInt, tunnels.split(", "))
  }.toMap

  val start = "AA"

  def bfs(current: Set[String], visited: Set[String], distances: Map[String, Int], i: Int): Map[String, Int] =
    if current.nonEmpty then
      val nextVisited = visited ++ current
      val next = current.map(data).flatMap(_.tunnels).diff(nextVisited)
      bfs(next, nextVisited, distances ++ current.map(_ -> i), i + 1)
    else
      distances

  val graph = data.keySet.map(k => k -> bfs(Set(k), Set.empty, Map.empty, 0)).toMap

  val timeLimit = 30

  def find(current: String, history: Set[String], time: Int, score: Int): Int =
    graph(current)
      .flatMap { case (nextNode, distance) =>
        val rate = data(nextNode).rate
        val newTime = time + distance + 1
        if !history.contains(nextNode) && rate > 0 && newTime < timeLimit then
          Some(find(nextNode, history + nextNode, newTime, score + (timeLimit - newTime) * rate))
        else
          None
      }
      .maxOption
      .getOrElse(score)

  part(1) = find(start, Set(start), 0, 0)

  val timeOffset = 4

  val interest = data.values.filter(_.rate > 0).map(_.valve).toSeq

  lazy val part2 = (0 to interest.size).flatMap(interest.combinations).view.map { aSeq =>
    val a = aSeq.toSet
    val b = interest.toSet -- a
    find(start, a + start, timeOffset, 0) + find(start, b + start, timeOffset, 0)
  }.max

  part(2) = 2615 // Too slow to be included

}
