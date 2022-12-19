import scala.collection.parallel.CollectionConverters._

case class Resources(ore: Int = 0, clay: Int = 0, obsidian: Int = 0, geode: Int = 0) {
  def plus(other: Resources): Resources = copy(
    ore = ore + other.ore,
    clay = clay + other.clay,
    obsidian = obsidian + other.obsidian,
    geode = geode + other.geode
  )

  def minus(other: Resources): Resources = copy(
    ore = ore - other.ore,
    clay = clay - other.clay,
    obsidian = obsidian - other.obsidian,
    geode = geode - other.geode
  )

  def isNonNegative: Boolean = ore >= 0 && clay >= 0 && obsidian >= 0 && geode >= 0
}

case class Blueprint(number: Int, oreRobot: Resources, clayRobot: Resources, obsidianRobot: Resources, geodeRobot: Resources)

object Blueprints {
  def of(input: List[String]): Blueprint = Blueprint(
      number = input(0) match { case s"Blueprint ${number}" => number.toInt },
      oreRobot = input(1) match { case s"Each ore robot costs ${ore} ore" => Resources(ore = ore.toInt) },
      clayRobot = input(2) match { case s"Each clay robot costs ${ore} ore" => Resources(ore = ore.toInt) },
      obsidianRobot = input(3) match { case s"Each obsidian robot costs ${ore} ore and ${clay} clay" => Resources(ore = ore.toInt, clay = clay.toInt)},
      geodeRobot = input(4) match { case s"Each geode robot costs ${ore} ore and ${obsidian} obsidian." => Resources(ore = ore.toInt, obsidian = obsidian.toInt)}
    )
}

val input: List[Blueprint] = common.loadPackets(List("day19.txt")).map(_.split("""[:.]\s""").toList).map(Blueprints.of)

case class State(minutes: Int = 1,
                 resources: Resources = Resources(),
                 robots: Resources = Resources(ore = 1),
                 blueprint: Blueprint,
                 maxMinutes: Int = 25) {
  def harvest: State = copy(resources = resources.plus(robots), minutes = minutes + 1)

  def canAfford(cost: Resources): Boolean = resources.minus(cost).isNonNegative

  def build(cost: Resources, robot: Resources): State = copy(resources = resources.minus(cost), robots = robots.plus(robot))

  def tryBuild(cost: Resources, robot: Resources): State =
    if (canAfford(cost))
      harvest.build(cost, robot)
    else harvest

  def buildOreRobot: Option[State] = {
    if (robots.clay == 0 && robots.obsidian == 0 && robots.geode == 0)
      LazyList.iterate(this)(s => s.tryBuild(blueprint.oreRobot, Resources(ore = 1)))
        .find(_.robots.ore == robots.ore + 1).filter(_.minutes <= maxMinutes && !canAfford(blueprint.geodeRobot))
    else None
  }

  def buildClayRobot: Option[State] = LazyList.iterate(this)(s => s.tryBuild(blueprint.clayRobot, Resources(clay = 1)))
    .find(_.robots.clay == robots.clay + 1).filter(_.minutes <= maxMinutes && !canAfford(blueprint.geodeRobot))

  def buildObsidianRobot: Option[State] =
    if (robots.clay > 0)
      LazyList.iterate(this)(s => s.tryBuild(blueprint.obsidianRobot, Resources(obsidian = 1)))
        .find(_.robots.obsidian == robots.obsidian + 1).filter(_.minutes <= maxMinutes && !canAfford(blueprint.geodeRobot))
    else None

  def buildGeodeRobot: Option[State] =
    if (robots.clay > 0 && robots.obsidian > 0)
      LazyList.iterate(this)(s => s.tryBuild(blueprint.geodeRobot, Resources(geode = 1)))
        .find(_.robots.geode == robots.geode + 1).filter(_.minutes <= maxMinutes)
    else None

  def justHarvest: State =
    LazyList.iterate(this)(s => s.harvest).find(_.minutes == maxMinutes).get

  def maxGeode: Int =
    if (minutes == maxMinutes)
      resources.geode
    else if (canAfford(blueprint.geodeRobot))
      harvest.build(blueprint.geodeRobot, Resources(geode = 1)).maxGeode
    else
      List(buildGeodeRobot, buildObsidianRobot, buildClayRobot, buildOreRobot)
        .filter(_.isDefined)
        .map(_.get.maxGeode)
        .sortBy(-_).headOption
        .getOrElse(justHarvest.maxGeode)
}


val part1 = input.map(blueprint => State(blueprint = blueprint).maxGeode * blueprint.number).sum
val part2 = input.take(3).par.map(blueprint => State(blueprint = blueprint, maxMinutes = 33).maxGeode)
  .product