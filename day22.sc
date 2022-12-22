val instructions = "10R5L5R10L4R5L5".split("(?<=\\d)(?!\\d)|(?<!\\d)(?=\\d)").toList
val map = common.loadPackets(List("day22.txt"))
type Facing = Int

val right = 0
val down = 1
val left = 2
val up = 3

case class Point(row: Int, col: Int) {
  def - (o: Point): Point = Point(row - o.row, col - o.col)
  def sign: Point = Point(row.sign, col.sign)

  def + (o: Point): Point = Point(row + o.row, col + o.col)
  def * (i: Int): Point = Point(row * i, col * i)
  def size: Int = row.abs + col.abs

  def step(facing: Facing): Point = facing match {
    case _ if facing == right => copy(col = col + 1)
    case _ if facing == down => copy(row = row + 1)
    case _ if facing == left => copy(col = col - 1)
    case _ if facing == up => copy(row = row - 1)
  }

  def charOnMap: Option[Char] =
    if (map.indices.contains(row - 1) && map(row - 1).indices.contains(col - 1))
      Some(map(row - 1)
        .charAt(col - 1))
        .filter(".#".contains(_))
    else
      None
}

case class Edge(from: Point, to: Point, outwardFacing: Int) {
  val points: Set[Point] = (0 to 3).map(i => from + (to - from).sign * i).toSet
}

def oppositeFacing(facing: Facing): Facing =
  if (facing == right) left
  else if (facing == left) right
  else if (facing == up) down
  else up

/*
 * Map edges of the map to the part they're glued onto.
 */
val edgePairs: List[Set[Edge]] = List(
  // A0 -> A1
  Set(Edge(Point(5, 5), Point(5, 8), up), Edge(Point(1, 9), Point(4, 9), right)),
  // B0 -> B1
  Set(Edge(Point(1, 9), Point(1, 12), up), Edge(Point(5, 4), Point(5, 1), down)),
  // C0 -> C1
  Set(Edge(Point(1, 12), Point(4, 12), right), Edge(Point(12, 16), Point(9, 16), right)),
  // D0 -> D1
  Set(Edge(Point(8, 12), Point(5, 12), right), Edge(Point(9, 13), Point(9, 16), up)),
  // E0 -> E1
  Set(Edge(Point(12, 16), Point(12, 13), down), Edge(Point(8, 6), Point(8, 8), down)),
  // F0 -> F1
  Set(Edge(Point(12, 9), Point(9, 9), left), Edge(Point(8, 5), Point(8, 8), down)),
  // G0 -> G1
  Set(Edge(Point(12, 9), Point(12, 12), down), Edge(Point(8, 4), Point(8, 1), down))
)

/**
 * Projects a point a on line a0, a1 to a point onto line b0, b1.
 * a0 gets projected onto b0, a1 onto b1. We do not actually need a1.
 */
def project(a: Point, source: Edge, target: Edge): Point =
  target.from + (target.to - target.from).sign * (source.from - a).size

case class MapCoord(p: Point, facing: Facing) {
  def turn(direction: Char): MapCoord = direction match {
    case 'L' => copy(facing = Math.floorMod(facing - 1, 4))
    case 'R' => copy(facing = Math.floorMod(facing + 1, 4))
  }
  def finalPassword: Int = 1000 * p.row + 4 * p.col + facing

  def step: MapCoord = copy(p = p.step(facing))

  def move(steps: Int): MapCoord = {
    if (steps == 0)
      this
    else {
      val next = step
      next.p.charOnMap match {
        case Some('.') => next.move(steps - 1)
        case Some('#') => this
        case None =>
          val moved = onMatchingEdge
          moved.p.charOnMap match {
            case Some('.') => moved.move(steps - 1)
            case Some('#') => this
          }
      }
    }
  }

  def onMatchingEdge: MapCoord = {
    val pair = edgePairs.find(_.exists(edge => edge.points.contains(p) && edge.outwardFacing == facing)).get
    val (here, there) = pair.partition(_.points.contains(p))
    val projected = project(p, here.head, there.head)
    MapCoord(projected, oppositeFacing(there.head.outwardFacing))
  }
}

val start = MapCoord(Point(1, 9), right)

val digits = """\d+""".r
def follow(coord: MapCoord, instruction: String): MapCoord = instruction match {
  case digits() => coord.move(instruction.toInt)
  case _ => coord.turn(instruction.charAt(0))
}

val part2 = instructions.foldLeft(start)(follow).finalPassword