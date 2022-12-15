package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day15 = Day(15) { (input, part) =>

  case class Vec(x: Int, y: Int):
    def manhattan(that: Vec): Int = (x - that.x).abs + (y - that.y).abs

  val data = input.split(lineSeparator).map {
    case s"Sensor at x=$xs, y=$ys: closest beacon is at x=$xb, y=$yb" => (Vec(xs.toInt, ys.toInt), Vec(xb.toInt, yb.toInt))
  }

  val scanX = 5000000
  val scanY = 2000000

  part(1) = (-scanX to scanX).count { x =>
    val p = Vec(x, scanY)
    data.forall((s, b) => s != p && b != p) && data.exists((a, b) => p.manhattan(a) <= a.manhattan(b))
  }

  val lines = data.flatMap { case (sensor, beacon) =>
    val d = sensor.manhattan(beacon) + 1
    for
      v <- Seq(d, -d)
      b <- Seq(false, true)
    yield (sensor.copy(x = sensor.x + v), b)
  }

  val groups = lines.groupBy(_._2)
  val boundaries = 0 to 4000000

  val v = (for {
    (p1, _) <- groups(false)
    (p2, _) <- groups(true)
    c1 = p1.y + p1.x
    c2 = p2.y - p2.x
    x = (c1 - c2) / 2
    y = x + c2
    p = Vec(x, y)
    if data.forall((a, b) => p.manhattan(a) > a.manhattan(b))
    if boundaries.contains(x) && boundaries.contains(y)
  } yield p).head

  part(2) = v.x * boundaries.max.toLong + v.y

}
