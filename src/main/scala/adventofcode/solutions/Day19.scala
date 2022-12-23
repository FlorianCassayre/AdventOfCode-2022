package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day19 = Day(19) { (input, part) =>

  enum Material:
    case Ore
    case Clay
    case Obsidian
    case Geode
  import Material.*

  val materials = Seq(Ore, Clay, Obsidian, Geode)
  val emptyMaterials = materials.map(_ -> 0).toMap

  case class Blueprint(id: Int, oreCostOre: Int, clayCostOre: Int, obsidianCostOre: Int, obsidianCostClay: Int, geodeCostOre: Int, geodeCostObsidian: Int):
    def costs: Map[Material, Map[Material, Int]] = Map(
      Ore -> Map(Ore -> oreCostOre),
      Clay -> Map(Ore -> clayCostOre),
      Obsidian -> Map(Ore -> obsidianCostOre, Clay -> obsidianCostClay),
      Geode -> Map(Ore -> geodeCostOre, Obsidian -> geodeCostObsidian)
    )

  val blueprints = input.split(lineSeparator).map {
    case s"Blueprint $id: Each ore robot costs $oreCostOre ore. Each clay robot costs $clayCostOre ore. Each obsidian robot costs $obsidianCostOre ore and $obsidianCostClay clay. Each geode robot costs $geodeCostOre ore and $geodeCostObsidian obsidian." =>
      Blueprint(id.toInt, oreCostOre.toInt, clayCostOre.toInt, obsidianCostOre.toInt, obsidianCostClay.toInt, geodeCostOre.toInt, geodeCostObsidian.toInt)
  }

  def solve(states: Set[(Map[Material, Int], Map[Material, Int])], time: Int, costs: Map[Material, Map[Material, Int]]): Int =
    if time > 0 then
      val geodesLowerBound = states.map(t => t._1(Geode) + t._2(Geode) * time).maxOption.getOrElse(0)
      val filteredMaxObtainable = states.filter { case (resources, robots) =>
        val currentRobots = robots(Geode)
        val maxObtainableGeodes = resources(Geode) + time * currentRobots + time * (time + 1) / 2
        maxObtainableGeodes > geodesLowerBound
      }
      val filteredSuboptimal = filteredMaxObtainable.groupBy(_._2).view.mapValues { values =>
        val resources = values.map(_._1)
        resources.filter(r1 => !resources.exists(r2 => r1 != r2 && r2.forall(t => r1(t._1) <= t._2)))
      }.flatMap(t => t._2.map(_ -> t._1)).toSet
      val filteredZeroed = time match
        case 3 => filteredSuboptimal.filter(t => t._2(Clay) > 0 || t._2(Obsidian) > 0 || t._2(Geode) > 0)
        case 2 => filteredSuboptimal.filter(t => t._2(Obsidian) > 0 || t._2(Geode) > 0)
        case 1 => filteredSuboptimal.filter(_._2(Geode) > 0)
        case _ => filteredSuboptimal

      val newStates = filteredZeroed.flatMap { case (resources, robots) =>
        val newResourcesAndRobotsAfterPurchase = materials
          .filter(material => costs(material).forall { case (material, amount) => resources(material) >= amount })
          .map(material =>
            (
              costs(material).foldLeft(resources) { case (acc, (material, amount)) => acc + (material -> (acc(material) - amount)) },
              robots + (material -> (robots(material) + 1))
            )
          )
        val couldPurchase = costs.values.count(cost => cost.keys.forall(robots.contains))
        val newResourcesAndRobots = if newResourcesAndRobotsAfterPurchase.size < couldPurchase then newResourcesAndRobotsAfterPurchase :+ (resources, robots) else newResourcesAndRobotsAfterPurchase
        newResourcesAndRobots.map { case (newResources, newRobots) =>
          val newResourcesAfterCollection = robots.foldLeft(newResources) { case (acc, (material, amount)) => acc + (material -> (acc(material) + amount)) }
          (newResourcesAfterCollection, newRobots)
        }
      }
      solve(newStates, time - 1, costs)
    else
      states.map(_._1(Geode)).maxOption.getOrElse(0)

  lazy val part1 = blueprints.map(blueprint => blueprint.id *
    solve(Set((emptyMaterials, emptyMaterials + (Ore -> 1))), 24, blueprint.costs)
  ).sum

  part(1) = 1616

  lazy val part2 = blueprints.take(3).map(blueprint =>
    solve(Set((emptyMaterials, emptyMaterials + (Ore -> 1))), 32, blueprint.costs)
  ).product

  part(2) = 8990

}
