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

  def direction(facing: Int): (Int, Int) = facing match
    case 0 => (0, 1)
    case 1 => (1, 0)
    case 2 => (0, -1)
    case 3 => (-1, 0)
    case _ => throw new Exception

  val diameter = Math.max(map.size, map.head.size)

  def nextPositionFlat(row: Int, column: Int, facing: Int): (Int, Int) =
    val (di, dj) = direction(facing)
    (1 until diameter).view
      .map(k => ((row + k * di + map.size) % map.size, (column + k * dj + map.head.size) % map.head.size))
      .find((i, j) => map(i)(j).nonEmpty)
      .filter((i, j) => map(i)(j).contains(false))
      .getOrElse((row, column))

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

  def nextPositionCube(row: Int, column: Int, facing: Int): (Int, Int) =
    ???

  def wander(cube: Boolean, row: Int = startRow, column: Int = startColumn, facing: Int = 0, instructions: Seq[Either[Int, Boolean]] = instructions): Int =
    instructions match
      case head +: tail =>
        head match
          case Left(steps) =>
            def move(row: Int, column: Int, i: Int): (Int, Int) =
              if i > 0 then
                val f = if cube then nextPositionCube else nextPositionFlat
                val (nextRow, nextColumn) = f(row, column, facing)
                move(nextRow, nextColumn, i - 1)
              else
                (row, column)
            val (newRow, newColumn) = move(row, column, steps)
            wander(cube, newRow, newColumn, facing, tail)
          case Right(turn) =>
            val newFacing = (facing + (if turn then 1 else -1) + 4) % 4
            wander(cube, row, column, newFacing, tail)
      case _ => 1000 * (row + 1) + 4 * (column + 1) + facing

  part(1) = wander(false)

  part(2) = wander(true)

}
