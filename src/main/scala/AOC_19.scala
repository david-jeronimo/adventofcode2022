import scala.annotation.tailrec
import scala.io.Source

object AOC_19 {

    def main(args: Array[String]): Unit =
        Seq(
            bestBlueprint(Source.fromResource("day19_sample.txt")),
            bestBlueprint(Source.fromResource("day19_input.txt")),
            bestBlueprintPartTwo(Source.fromResource("day19_sample.txt")),
            bestBlueprintPartTwo(Source.fromResource("day19_input2.txt")),
        ).foreach(println)

    sealed trait Resource { def value: Int}
    case object Ore extends Resource { val value = 1}
    case object Clay extends Resource { val value = 2 }
    case object Obsidian extends Resource { val value = 3 }
    case object Geode extends Resource { val value = 4 }

    case class RobotCost(resource: Resource, cost: List[(Resource, Int)])
    case class Blueprint(id: Int, robotCosts: List[RobotCost])

    type Robot = Resource
    case class State(robots: List[Robot], resources: Map[Resource, Int], logs: List[String])

    implicit def strToResource(str: String): Resource =
        str match {
            case "ore" => Ore
            case "clay" => Clay
            case "obsidian" => Obsidian
            case "geode" => Geode
        }

    def bestBlueprint(source: Source): Int = {
        val blueprints = source.getLines().map(lineToBlueprint).toList
        val initState = State(List(Ore), Map.empty, List())
        blueprints
          .map(bp => (bp.id, collectGeodes(List(initState), bp, 24)))
          .map(e => e._1 * e._2)
          .sum
    }

    def bestBlueprintPartTwo(source: Source): Int = {
        val blueprints = source.getLines().map(lineToBlueprint).toList
        val initState = State(List(Ore), Map.empty, List())
        blueprints
          .map(bp => (bp.id, collectGeodes(List(initState), bp, 32)))
          .map(e => e._2)
          .product
    }

    def lineToBlueprint(line: String): Blueprint =
        line.split(": ") match {
            case Array(blueprintStr, robotCostsStr) =>
                val bluePrintId: Int = blueprintStr.split("print ")(1).toInt
                val robotCosts = robotCostsStr.dropRight(1).split(". E").map(
                    robotCostStr => {
                        val segments = robotCostStr.split(" ")
                        val robotResource: Resource = segments(1)
                        val quantity: Int = segments(4).toInt
                        val quantityResource: Resource = segments(5)
                        val cost1: List[(Resource, Int)] = List(quantityResource -> quantity)
                        val cost: List[(Resource, Int)] = if (segments.length > 6) {
                            val resource2: Resource = segments(8)
                            cost1 :+ (resource2 -> segments(7).toInt)
                        } else cost1
                        RobotCost(robotResource, cost)
                    }
                )
                Blueprint(bluePrintId, robotCosts.toList)
        }

    @tailrec
    def collectGeodes(states: List[State], blueprint: Blueprint, count: Int): Int = {
        val sortedStates = states.sortBy(st => (
          -st.resources.getOrElse(Geode, 0), -st.robots.count(_ == Geode),
          -st.resources.getOrElse(Obsidian, 0), -st.robots.count(_ == Obsidian),
          -st.resources.getOrElse(Clay, 0),
        )).take(10_000)
        (count, sortedStates) match {
            case (0, st::_) =>
                st.resources.getOrElse(Geode, 0)
            case (_, sts)=>
                val nextSt = sts.flatMap(nextStates(_, blueprint, count))
                collectGeodes(nextSt, blueprint, count - 1)
        }
    }

    def nextStates(state: State, blueprint: Blueprint, count: Int): List[State] = {
        val gatheredResources = state.robots.groupBy(r => r).map(e => e._1 -> e._2.length)
        val resources = state.resources ++ gatheredResources.map {
            case (k, v) => k -> (v + state.resources.getOrElse(k, 0))
        }
        state.copy(resources = resources) +: buildRobotActions(state, blueprint)
          .map(robot => {
                val robotCost = blueprint.robotCosts.find(rc => rc.resource == robot).get
                State(robot +: state.robots, spendResources(robotCost, resources), s"$count}: build $robot" +: state.logs)
            }
        )
    }

    def buildRobotActions(state: State, blueprint: Blueprint): List[Robot] = {
        def canBeBuilt(robotCost: RobotCost): Boolean =
            robotCost.cost.forall {
                case (resource, amount) => state.resources.getOrElse(resource, 0) >= amount
            }

        blueprint.robotCosts
          .filter(canBeBuilt)
          .sortBy(-_.resource.value)
          .map(_.resource)
    }

    def spendResources(robotCost: RobotCost, resources: Map[Resource, Int]): Map[Resource, Int] =
        resources ++ robotCost.cost.map {
            case (k, v) => k -> (resources.getOrElse(k, 0) - v)
        }

}
