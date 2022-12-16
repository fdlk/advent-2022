import common.{loadPackets, aStarSearch, Grid}

type ValveId = String
case class Valve(id: ValveId, flowRate: Int, tunnels: List[ValveId])

val regex = """^Valve ([A-Z]{2}) has flow rate=(\d+); tunnels? leads? to valves? (.*)""".r

val valves = loadPackets(List("day16.txt")).map({
  case regex(valve, flow, tunnels) => Valve(valve, flow.toInt, tunnels.split(", ").toList)
}).map(v => v.id -> v).toMap

val tunnels = new Grid[ValveId]() {
  override def heuristicDistance(from: ValveId, to: ValveId): Int = 0

  override def getNeighbours(state: ValveId): Iterable[ValveId] = valves(state).tunnels

  override def moveCost(from: ValveId, to: ValveId): Int = 1
}

val toVisit = valves.values.filter(_.flowRate > 0).toList.sortBy(-_.flowRate).map(_.id)
val distances = ("AA"::toVisit).combinations(2).map(combination => combination.toSet -> aStarSearch(combination(0), combination(1), tunnels).get).toMap

case class State(minutesLeft: Int = 30, score: Int = 0, location: ValveId = "AA", closed: Set[ValveId] = toVisit.toSet, visited: List[ValveId] = List()) {
  def openValve(valve: ValveId): Option[State] = {
    val minutes = if (location == valve) minutesLeft - 1 else minutesLeft - 1 - distances(Set(location, valve))
    if (minutes < 0) None
    else Some(copy(
      minutesLeft = minutes,
      score = score + minutes * valves(valve).flowRate,
      location = valve,
      closed = closed - valve,
      visited = valve :: visited
    ))
  }

  val done: Boolean = minutesLeft <= 1 || closed.isEmpty

  val maxScoreMogelijk: Int =
    if (done) score
    else score + closed.map(minutesLeft * valves(_).flowRate).sum
}

def part1(state: State, toVisit: List[ValveId], bestScoreSoFar: Int = 0): Int = {
  if (state.maxScoreMogelijk < bestScoreSoFar)
    bestScoreSoFar
  else if (toVisit.isEmpty) {
    bestScoreSoFar.max(state.score)
  }
  else {
    val next = state.openValve(toVisit.head)
    if (next.isEmpty) {
      bestScoreSoFar.max(state.score).max(part1(state, toVisit.tail, bestScoreSoFar))
    } else {
      val scoreWithNext = part1(next.get, next.get.closed.toList.sortBy(-valves(_).flowRate), bestScoreSoFar)
      scoreWithNext.max(part1(state, toVisit.tail, scoreWithNext))
    }
  }
}

part1(State(), toVisit)

def partition(toVisit: List[ValveId]): List[(List[ValveId], List[ValveId])] = {
  if (toVisit.isEmpty) List((List(), List()))
  else {
    partition(toVisit.tail)
      .flatMap({case (me: List[ValveId], elephant: List[ValveId]) =>
        List((toVisit.head :: me, elephant), (me, toVisit.head :: elephant))})
  }
}

val options = partition(toVisit)

val part2 = options.map({case (me, elephant) => part1(State(minutesLeft = 26, closed = me.toSet), me) +
    part1(State(minutesLeft = 26, closed = elephant.toSet), elephant)}).max