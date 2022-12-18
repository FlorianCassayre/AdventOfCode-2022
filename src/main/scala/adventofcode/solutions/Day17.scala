package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day17 = Day(17) { (input, part) =>

  case class Vec(x: Long, y: Long):
    def +(that: Vec): Vec = Vec(x + that.x, y + that.y)

  val shapes =
    """####
      |
      |.#.
      |###
      |.#.
      |
      |..#
      |..#
      |###
      |
      |#
      |#
      |#
      |#
      |
      |##
      |##""".stripMargin
      .split(lineSeparator * 2)
      .map(_.split(lineSeparator).map(_.map({
        case '#' => true
        case '.' => false
      }))).toSeq

  val jet = input.trim.map {
    case '<' => false
    case '>' => true
  }

  def move(map: Set[Vec], shape: Set[Vec], direction: Vec): (Boolean, Set[Vec]) =
    val nextShape = shape.map(_ + direction)
    if nextShape.intersect(map).isEmpty && nextShape.forall(v => v.y <= 0 && v.x >= -3 && v.x <= 3) then (true, nextShape) else (false, shape)

  def fall(map: Set[Vec], shape: Set[Vec], jetIndex: Int): (Set[Vec], Int) = {
    val (_, nextHorizontal) = move(map, shape, Vec(if jet(jetIndex) then 1 else -1, 0))
    val (success, nextVertical) = move(map, nextHorizontal, Vec(0, 1))
    val nextJetIndex = (jetIndex + 1) % jet.size
    if success then
      fall(map, nextVertical, nextJetIndex)
    else
      (map ++ nextVertical, nextJetIndex)
  }

  def normalize(map: Set[Vec]): Set[Vec] =
    map.map(_.y).maxOption match
      case Some(maxY) => map.map(v => v.copy(y = v.y - maxY))
      case None => map

  def simulate2(i: Long, maxI: Long, jetIndex: Int, map: Set[Vec], cache: Map[(Set[Vec], Int), (Long, Long)]): Long =
    if i < maxI then
      val minY = map.map(_.y).minOption.getOrElse(1L)
      val historySize = 10
      val line = (normalize(map.filter(v => v.y < minY + historySize)), jetIndex)
      cache.get(line)
        .map { case (pastI, pastY) => (i - pastI, pastY - minY) }
        .filter { case (dI, _) => (maxI - i) > dI }
      match
        case Some((dI, dY)) =>
          val loops = (maxI - i) / dI
          simulate2(i + dI * loops, maxI, jetIndex, map.map(v => v.copy(y = v.y - dY * loops)), cache)
        case None =>
          val shapeArray = shapes((i % shapes.size).toInt)
          val shape = for
            y0 <- shapeArray.indices
            x0 <- shapeArray(y0).indices
            if shapeArray(y0)(x0)
            y = minY + y0 - shapeArray.length - 4 + 1
            x = x0 - 1
            v = Vec(x, y)
          yield v
          val (nextMap, nextJetIndex) = fall(map, shape.toSet, jetIndex)
          simulate2(i + 1, maxI, nextJetIndex, nextMap, cache + (line -> (i, minY)))
    else
      map.map(_.y).min.abs + 1

  part(1) = simulate2(0, 2022, 0, Set.empty, Map.empty)

  part(2) = simulate2(0, 1000000000000L, 0, Set.empty, Map.empty)

}
