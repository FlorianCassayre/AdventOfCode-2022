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

  def solve(states: Set[(Map[Material, Int], Map[Material, Int], Set[Material])], time: Int, costs: Map[Material, Map[Material, Int]]): Int =
    if time > 0 then
      val maxRobots = costs.values.toSeq.flatMap(_.toSeq).groupBy(_._1).view.mapValues(_.map(_._2).max).toMap
      val geodesLowerBound = states.map(t => t._1(Geode) + t._2(Geode) * time).maxOption.getOrElse(0)
      val filteredMaxObtainable = states.filter { case (resources, robots, _) =>
        val currentRobots = robots(Geode)
        val maxObtainableGeodes = resources(Geode) + time * currentRobots + time * (time + 1) / 2
        maxObtainableGeodes > geodesLowerBound
      }
      val filteredSuboptimal = filteredMaxObtainable.groupBy(t => (t._2, t._3)).view.mapValues { values =>
        val resources = values.map(_._1)
        resources.filter(r1 => !resources.exists(r2 => r1 != r2 && r2.forall(t => r1(t._1) <= t._2)))
      }.flatMap(t => t._2.map(v => (v, t._1._1, t._1._2))).toSet

      val newStates = filteredSuboptimal.flatMap { case (resources, robots, refused) =>
        val purchasableRobots = materials
          .filter(material => !refused.contains(material) && costs(material).forall { case (material, amount) => resources(material) >= amount })
        val newResourcesAndRobotsAfterPurchase = purchasableRobots
          .map(material =>
            (
              costs(material).foldLeft(resources) { case (acc, (material, amount)) => acc + (material -> (acc(material) - amount)) },
              robots + (material -> (robots(material) + 1)),
              Set.empty[Material]
            )
          )
        val actualNewResourcesAndRobotsAfterPurchase = newResourcesAndRobotsAfterPurchase.filter { case (_, newRobots, _) =>
          newRobots.forall { case (material, quantity) => !maxRobots.contains(material) || quantity <= maxRobots(material) }
        }
        val couldPurchase = costs.values.count(cost => cost.keys.forall(robots.contains))
        val newResourcesAndRobots =
          if newResourcesAndRobotsAfterPurchase.size < couldPurchase then
            actualNewResourcesAndRobotsAfterPurchase :+ (resources, robots, refused ++ purchasableRobots.toSet)
          else
            actualNewResourcesAndRobotsAfterPurchase
        newResourcesAndRobots.map { case (newResources, newRobots, newRefused) =>
          val newResourcesAfterCollection = robots.foldLeft(newResources) { case (acc, (material, amount)) => acc + (material -> (acc(material) + amount)) }
          (newResourcesAfterCollection, newRobots, newRefused)
        }
      }
      solve(newStates, time - 1, costs)
    else
      states.map(_._1(Geode)).maxOption.getOrElse(0)

  val initialState = Set((emptyMaterials, emptyMaterials + (Ore -> 1), Set.empty[Material]))

  part(1) = blueprints.map(blueprint => blueprint.id * solve(initialState, 24, blueprint.costs)).sum

  part(2) = blueprints.take(3).map(blueprint => solve(initialState, 32, blueprint.costs)).product

}
