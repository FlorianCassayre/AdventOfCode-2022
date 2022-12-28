package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day22 = Day(22) { (input, part) =>

  val (map, instructions) =
    val parts = input.split(lineSeparator * 2)
    val map = parts.head.split(lineSeparator)
    val width = map.map(_.length).max
    val binaryMap = map.map(_.map {
      case ' ' => None
      case '.' => Some(false)
      case '#' => Some(true)
    }).map(row => row ++ Seq.fill(width - row.size)(None)).map(_.toIndexedSeq).toIndexedSeq
    val L = "L"
    val R = "R"
    val seq = Seq(L, R).foldLeft(parts.tail.head)((s, c) => s.replace(c, s" $c ")).trim.split(" ").map {
      case `L` => Right(false)
      case `R` => Right(true)
      case s => Left(s.toInt)
    }
    (binaryMap, seq)

  val direction = IndexedSeq((0, 1), (1, 0), (0, -1), (-1, 0))

  val diameter = Math.max(map.size, map.head.size)

  def nextPositionFlat(row: Int, column: Int, facing: Int): ((Int, Int), Int) =
    val (di, dj) = direction(facing)
    val result = (1 until diameter).view
      .map(k => ((row + k * di + map.size) % map.size, (column + k * dj + map.head.size) % map.head.size))
      .find((i, j) => map(i)(j).nonEmpty)
      .filter((i, j) => map(i)(j).contains(false))
      .getOrElse((row, column))
    (result, facing)

  val (startRow, startColumn) = (
    for
      i <- map.indices
      j <- map(i).indices
      if map(i)(j).contains(false)
    yield (i, j)
    ).head

  val faceSize =
    def maxSize(array: IndexedSeq[IndexedSeq[Option[Boolean]]]): Int =
      array.map(_.view.dropWhile(_.isEmpty).takeWhile(_.nonEmpty).size).min
    maxSize(map).min(maxSize(map.transpose))

  val (metaWidth, metaHeight) = (map.head.size / faceSize, map.size / faceSize)
  val meta = (0 until metaHeight).map(i => (0 until metaWidth).map(j => map(i * faceSize)(j * faceSize).nonEmpty))

  // This is specific to the input
  val warpMap: Map[(Int, Int), Map[Int, ((Int, Int), Int)]] = Map(
    (0, 1) -> Map(
      2 -> ((2, 0), 0),
      3 -> ((3, 0), 0),
    ),
    (0, 2) -> Map(
      0 -> ((2, 1), 2),
      1 -> ((1, 1), 2),
      3 -> ((3, 0), 3),
    ),
    (1, 1) -> Map(
      2 -> ((2, 0), 1),
    ),
    (2, 1) -> Map(
      1 -> ((3, 0), 2),
    )
  )
  val fullWarpMap =
    val reversed = warpMap.toSeq.flatMap((location, directions) => directions.toSeq.map(location -> _))
      .map { case (from, (fromDirection, (to, toDirection))) =>
        to -> ((toDirection + 2) % 4, (from, (fromDirection + 2) % 4))
      }.groupBy(_._1).view.mapValues(_.map(_._2).toMap).toMap
    (warpMap.keys ++ reversed.keys).toSet.map(key => key -> (warpMap.getOrElse(key, Map.empty) ++ reversed.getOrElse(key, Map.empty))).toMap

  def nextPositionCube(row: Int, column: Int, facing: Int): ((Int, Int), Int) =
    val (di, dj) = direction(facing)
    val (previousMetaI, previousMetaJ) = (Math.floorDiv(row, faceSize), Math.floorDiv(column, faceSize))
    val (nextRow, nextColumn) = (row + di, column + dj)
    val (nextMetaI, nextMetaJ) = (Math.floorDiv(nextRow, faceSize), Math.floorDiv(nextColumn, faceSize))
    val actual =
      if (previousMetaI == nextMetaI && previousMetaJ == nextMetaJ) || (meta.indices.contains(nextMetaI) && meta(nextMetaI).indices.contains(nextMetaJ) && meta(nextMetaI)(nextMetaJ)) then
        ((nextRow, nextColumn), facing)
      else
        val ((actualMetaI, actualMetaJ), newFacing) = fullWarpMap((previousMetaI, previousMetaJ))(facing)
        val (mI, mJ) = ((nextRow + faceSize) % faceSize, (nextColumn + faceSize) % faceSize)
        def rotateCW(i: Int, j: Int, k: Int): (Int, Int) = if k > 0 then rotateCW(j, faceSize - i - 1, k - 1) else (i, j)
        val (rI, rJ) = rotateCW(mI, mJ, newFacing - facing + 4)
        ((actualMetaI * faceSize + rI, actualMetaJ * faceSize + rJ), newFacing)
    val ((actualNextRow, actualNextColumn), _) = actual
    if map(actualNextRow)(actualNextColumn).contains(false) then actual else ((row, column), facing)

  def wander(cube: Boolean, row: Int = startRow, column: Int = startColumn, facing: Int = 0, instructions: Seq[Either[Int, Boolean]] = instructions): Int =
    instructions match
      case head +: tail =>
        head match
          case Left(steps) =>
            def move(row: Int, column: Int, facing: Int, i: Int): ((Int, Int), Int) =
              if i > 0 then
                val f = if cube then nextPositionCube else nextPositionFlat
                val ((nextRow, nextColumn), newFacing) = f(row, column, facing)
                move(nextRow, nextColumn, newFacing, i - 1)
              else
                ((row, column), facing)
            val ((newRow, newColumn), newFacing) = move(row, column, facing, steps)
            wander(cube, newRow, newColumn, newFacing, tail)
          case Right(turn) =>
            val newFacing = (facing + (if turn then 1 else -1) + 4) % 4
            wander(cube, row, column, newFacing, tail)
      case _ => 1000 * (row + 1) + 4 * (column + 1) + facing

  part(1) = wander(false)

  part(2) = wander(true)

}
