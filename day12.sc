import common.{loadPackets, aStarSearch, Grid}

val input = loadPackets(List("day12.txt"))

case class Point(x: Int, y: Int) {
  lazy val letter: Char = input(y).charAt(x)
  lazy val height: Char = letter match {
    case 'S' => 'a'
    case 'E' => 'z'
    case x => x
  }
  lazy val isWithinBounds: Boolean = input.indices.contains(y) && input.head.indices.contains(x)

  def plus(direction: Point): Point = Point(x + direction.x, y + direction.y)

  def neighbours: List[Point] = List(Point(-1, 0), Point(1, 0), Point(0, 1), Point(0, -1))
    .map(plus)
    .filter(_.isWithinBounds)

  def distance(other: Point): Int = (x - other.x).abs + (y - other.y).abs
}

val hill = for (
  y <- input.indices;
  x <- input.head.indices
) yield Point(x, y)

val grid: Grid[Point] = new Grid[Point] {
  override def heuristicDistance(from: Point, to: Point): Int = from.distance(to)

  override def getNeighbours(state: Point): Iterable[Point] = state.neighbours
    .filter(p => p.height - state.height <= 1)

  override def moveCost(from: Point, to: Point): Int = 1
}

val start: Point = hill.find(_.letter == 'S').get
val end: Point = hill.find(_.letter == 'E').get

val part1 = aStarSearch(start, end, grid).get
val part2 = hill.filter(_.height == 'a').flatMap(aStarSearch(_, end, grid)).min