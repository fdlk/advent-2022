import scala.collection.parallel.CollectionConverters._
import scala.annotation.tailrec

sealed abstract class Resource
case object Ore extends Resource
case object Clay extends Resource
case object Obsidian extends Resource
case object Geode extends Resource

case class Resources(ore: Int = 0, clay: Int = 0, obsidian: Int = 0, geode: Int = 0) {
  def get(resource: Resource) = resource match {
    case Ore => ore
    case Clay => clay
    case Obsidian => obsidian
    case Geode => geode
  }
  def has(resource: Resource): Boolean = resource match {
    case Ore => ore > 0
    case Clay => clay > 0
    case Obsidian => obsidian > 0
    case Geode => geode > 0
  }
  def +(resource: Resource): Resources = resource match {
    case Ore => copy(ore = ore + 1)
    case Clay => copy(clay = clay + 1)
    case Obsidian => copy(obsidian = obsidian + 1)
    case Geode => copy(geode = geode + 1)
  }
  def +(other: Resources): Resources = copy(ore = ore + other.ore, clay = clay + other.clay, obsidian = obsidian + other.obsidian, geode = geode + other.geode)
  def -(other: Resources): Resources = copy(ore = ore - other.ore, clay = clay - other.clay, obsidian = obsidian - other.obsidian, geode = geode - other.geode)
  def >=(other: Resources): Boolean = ore >= other.ore && clay >= other.clay && obsidian >= other.obsidian && geode >= other.geode
}

case class Blueprint(number: Int, costs: Map[Resource, Resources]) {
  def maxCost(resource: Resource): Int = costs.values.map(_.get(resource)).max
}

object Blueprints {
  def of(input: List[String]): Blueprint = Blueprint(
    number = input.head match {
      case s"Blueprint ${number}" => number.toInt
    },
    costs = Map(
      input(1) match { case s"Each ore robot costs ${ore} ore"                             => Ore -> Resources(ore = ore.toInt) },
      input(2) match { case s"Each clay robot costs ${ore} ore"                            => Clay -> Resources(ore = ore.toInt) },
      input(3) match { case s"Each obsidian robot costs ${ore} ore and ${clay} clay"       => Obsidian -> Resources(ore = ore.toInt, clay = clay.toInt) },
      input(4) match { case s"Each geode robot costs ${ore} ore and ${obsidian} obsidian." => Geode -> Resources(ore = ore.toInt, obsidian = obsidian.toInt) }
    ))
}

val input: List[Blueprint] = common.loadPackets(List("day19.txt")).map(_.split("""[:.]\s""").toList).map(Blueprints.of)

case class State(minutes: Int = 1,
                 resources: Resources = Resources(),
                 robots: Resources = Resources(ore = 1),
                 blueprint: Blueprint,
                 maxMinutes: Int = 25) {
  def harvest: State = copy(resources = resources + robots, minutes = minutes + 1)

  def canAfford(resource: Resource): Boolean = resources >= blueprint.costs(resource)

  def buildNow(resource: Resource): State =
    copy(resources = resources - blueprint.costs(resource), robots = robots + resource)

  @tailrec
  final def buildNext(resource: Resource): Option[State] =
    if (minutes == maxMinutes) None
    else if (canAfford(resource)) Some(harvest.buildNow(resource))
    else harvest.buildNext(resource)

  @tailrec
  final def justHarvest: State =
    if (minutes == maxMinutes) this
    else harvest.justHarvest

  def maxGeode: Int = {
    if (minutes == maxMinutes)
      resources.geode
    else if (canAfford(Geode))
      harvest.buildNow(Geode).maxGeode
    else
      List(Geode, Obsidian, Clay, Ore)
        .filter(resource => resource == Geode || robots.get(resource) < blueprint.maxCost(resource))
        .filter({
          case Obsidian => robots.has(Clay)
          case Geode => robots.has(Obsidian)
          case _ => true
        })
        .map(buildNext)
        .filter(_.isDefined)
        .map(_.get.maxGeode)
        .sorted.reverse.headOption
        .getOrElse(justHarvest.resources.geode)
  }
}

val part1 = input.map(blueprint => State(blueprint = blueprint).maxGeode * blueprint.number).sum
val part2 = input.take(3).par.map(blueprint => State(blueprint = blueprint, maxMinutes = 33).maxGeode).product