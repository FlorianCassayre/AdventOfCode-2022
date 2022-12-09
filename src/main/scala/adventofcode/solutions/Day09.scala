package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day09 = Day(9) { (input, part) =>

  case class Vector(x: Int, y: Int):
    def +(that: Vector): Vector = Vector(x + that.x, y + that.y)
    def -(that: Vector): Vector = Vector(x - that.x, y - that.y)
    def /(v: Int): Vector = Vector(x / v, y / v)
    def distance(that: Vector): Int = Math.max(Math.abs(x - that.x), Math.abs(y - that.y))

  val instructions = input.split(lineSeparator).map {
    case s"$direction $count" =>
      val vector = direction match
        case "U" => Vector(0, -1)
        case "R" => Vector(1, 0)
        case "D" => Vector(0, 1)
        case "L" => Vector(-1, 0)
      (vector, count.toInt)
  }

  def step(rope: Seq[Vector], direction: Vector): Seq[Vector] = {
    rope.tail.foldLeft(Seq(rope.head + direction)) {
      case (rope, node) =>
        val parent = rope.head
        val distance = parent.distance(node)
        val newNode = if distance > 1 then parent - (parent - node) / distance else node
        newNode +: rope
    }.reverse
  }

  def simulate(size: Int): Int =
    val origin = Vector(0, 0)
    instructions.foldLeft((Seq.fill(size)(origin), Set(origin))) { case ((rope, visited), (direction, count)) =>
      (0 until count).foldLeft((rope, visited)) { case ((r, v), _) =>
        val nr = step(r, direction)
        (nr, v + nr.last)
      }
    }._2.size

  part(1) = simulate(2)

  part(2) = simulate(10)

}
