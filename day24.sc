import common.{loadPackets, Grid, aStarSearch2}

val input = loadPackets(List("day24.txt"))
val width = input.head.length - 2
val height = input.size - 2
def lcm(a: Int, b: Int): Int =
  b * a / LazyList.iterate((a, b)) { case (x, y) => (y, x % y) }.dropWhile(_._2 != 0).head._1.abs

val period = lcm(width, height)

case class Point(row: Int, col: Int) {
  def move(direction: Char): Point = direction match {
    case '>' => copy(col = col + 1)
    case '<' => copy(col = col - 1)
    case '^' => copy(row = row - 1)
    case 'v' => copy(row = row + 1)
    case '.' => this
  }

  val isAccessible: Boolean =
    input.indices.contains(row) &&
    input(row).indices.contains(col) &&
    input(row).charAt(col) != '#'

  def -(other: Point): Point = copy(row = row - other.row, col = col - other.col)

  def size: Int = row.abs + col.abs

  def wrap: Point = {
    if (row == 0) copy(row = height)
    else if (row == height + 1) copy(row = 1)
    else if (col == 0) copy(col = width)
    else if (col == width + 1) copy(col = 1)
    else this
  }
}

Point(3,4).move('v').wrap

type Blizzards = List[(Point, Char)]
val directions = "><^v"

val blizzardStart: Blizzards = (for (
  row <- input.indices;
  col <- input.head.indices;
  c = input(row).charAt(col)
  if directions.contains(c)
) yield Point(row, col) -> c).toList

def update(blizzards: Blizzards): Blizzards = blizzards.map({
  case (point, direction) => (point.move(direction).wrap, direction)
})

val blizzards: List[Set[Point]] = LazyList.iterate(blizzardStart)(update)
  .take(period).map(_.map(_._1).toSet).toList

val start: Point = Point(0, input.head.indexOf('.'))
val end: Point = Point(height + 1, input(height + 1).indexOf('.'))
case class State(pos: Point = start, minutes: Int = 0) {
  val isSnowFree: Boolean = !blizzards(minutes % period).contains(pos)

  def move(direction: Char): Option[State] =
    Some(copy(pos = pos.move(direction), minutes = minutes + 1))
      .filter(_.pos.isAccessible)
      .filter(_.isSnowFree)

  def printMap: String = (0 to height + 1).map(row =>
    (0 to width + 1).map(col => {
      val p = Point(row, col)
      if (!p.isAccessible)
        '#'
      else if (blizzards(minutes % period).contains(p))
        '*'
      else if (p == pos)
        '@'
      else
        '.'
    }).mkString).mkString("\n")
}

val grid = new Grid[State] {
  override def heuristicDistance(from: State, to: State): Int = (from.pos - to.pos).size

  override def getNeighbours(state: State): Iterable[State] = "><^v.".flatMap(state.move)

  override def moveCost(from: State, to: State): Int = 1
}

aStarSearch2[State](State(), State(pos = end), grid, _.pos == end)