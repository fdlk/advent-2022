case class Point(x: Int, y: Int) {
  def plus(diff: Point): Point = Point(x + diff.x, y + diff.y)
}

case class Block(shape: List[String]) {
  val points: List[Point] = shape.indices.flatMap(y =>
    shape.head.indices.filter(x =>
      shape(shape.length - y - 1).charAt(x) == '#')
      .map(x => Point(x, y))).toList

  def at(bottomLeft: Point): List[Point] = points.map(_.plus(bottomLeft))
}

val blocks = List(
  Block(List("####")),
  Block(List(".#.", "###", ".#.")),
  Block(List("..#", "..#", "###")),
  Block(List("#", "#", "#", "#")),
  Block(List("##", "##"))
)

val gas = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

case class State(blockIndex: Int = 0,
                 gasIndex: Int = 0,
                 height: Int = 0,
                 bottomLeft: Point = Point(3, 4),
                 rock: List[Point] = List()) {
  val fallingBlockPoints: List[Point] = blocks(blockIndex % blocks.length).at(bottomLeft)
  val blockInRock: Boolean = fallingBlockPoints.exists(p => isWall(p) || rock.contains(p))

  def isWall(p: Point): Boolean = p.y == 0 || p.x == 0 || p.x == 8

  def charAt(p: Point): Char = p match {
    case _ if isWall(p) => '#'
    case _ if rock.contains(p) => '#'
    case _ if fallingBlockPoints.contains(p) => '@'
    case _ => '.'
  }

  def mkString: String = (fallingBlockPoints.map(_.y).max to 0 by -1)
    .map(y => (0 to 8)
      .map(x => Point(x, y)).map(charAt).mkString)
    .mkString("\n")

  def blow: State = {
    val jetDirection: Point = gas(gasIndex) match {
      case '>' => Point(1, 0)
      case '<' => Point(-1, 0)
    }
    val blown = copy(bottomLeft = bottomLeft.plus(jetDirection), gasIndex = (gasIndex + 1) % gas.length)
    if (blown.blockInRock)
      copy(gasIndex = (gasIndex + 1) % gas.length)
    else
      blown
  }

  def newBlock: State = copy(
    blockIndex = blockIndex + 1,
    bottomLeft = Point(3, height + 4)
  )

  def drop: State = {
    val dropped: State = copy(bottomLeft = bottomLeft.plus(Point(0, -1)))
    if (dropped.blockInRock) {
      copy(rock = rock.filter(_.y >= height - 50) ++ fallingBlockPoints,
        height = (height :: fallingBlockPoints.map(_.y)).max).newBlock
    } else
      dropped
  }

  def next: State = blow.drop
}

val before = LazyList.iterate(State())(_.next)
  .find(_.blockIndex == 2022)
  .get
val part1 = before.height

val period = gas.length * blocks.length
val after = LazyList.iterate(before)(_.next).drop(period).head
val blockDiff = after.blockIndex - before.blockIndex
val heightDiff = after.height - before.height
val modulo = 1000000000000L % blockDiff

val inSync = LazyList.iterate(after)(_.next)
  .find(_.blockIndex % blockDiff == modulo).get

val part2 = inSync.height + ((1000000000000L - inSync.blockIndex) / blockDiff) * heightDiff