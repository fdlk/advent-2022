import common.{loadPackets, aStarSearch, Grid}

case class Point(x: Int, y: Int, z: Int) {
  def add(o: Point): Point = Point(x + o.x, y + o.y, z + o.z)
  def diff(o: Point) = Point(x - o.x, y - o.y, z - o.z)
  def size: Int = x.abs + y.abs + z.abs
}

object Points {
  val directions: List[Point] = List(
    Point(-1, 0, 0), Point(1, 0, 0),
    Point(0, -1, 0), Point(0, 1, 0),
    Point(0, 0, -1), Point(0, 0, 1)
  )

  def of(line: String): Point = line.split(",").toList match {
    case List(x,y,z) => Point(x.toInt,y.toInt,z.toInt)
  }

  def neighbors(p: Point): List[Point] = directions.map(p.add)
}

val input: Set[Point] = loadPackets(List("day18.txt")).map(Points.of).toSet
val part1 = input.toList.map(Points.neighbors(_).count(!input.contains(_))).sum

val grid = new Grid[Point]() {
  override def heuristicDistance(from: Point, to: Point): Int = from.diff(to).size
  override def getNeighbours(state: Point): Iterable[Point] = Points.directions.map(state.add).filterNot(input.contains)
  override def moveCost(from: Point, to: Point): Int = 1
}
val outside: Point = Point(input.map(_.x).min - 1, input.map(_.y).min - 1, input.map(_.z).min - 1)
def isInnerBubble(p: Point): Boolean = aStarSearch(p, outside, grid).isEmpty

val part2 = input.toList.map(Points.neighbors(_).filterNot(input.contains).count(!isInnerBubble(_))).sum