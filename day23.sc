val input = common.loadPackets(List("day23.txt"))

case class Point(x: Int, y: Int) {
  def move(direction: String): Point = direction.toList match {
    case List('N') => copy(x = x - 1)
    case List('S') => copy(x = x + 1)
    case List('E') => copy(y = y + 1)
    case List('W') => copy(y = y - 1)
    case List(a, b) => move(a.toString).move(b.toString)
  }
}

val allDirections = List("NW", "N", "NE", "E", "SE", "S", "SW", "W")
val orthogonal = List("N", "S", "W", "E")

val start: Set[Point] = (for(x <- input.indices;
    y <- input(x).indices if input(x)(y) == '#')
  yield Point(x, y)).toSet

case class State(elves: Set[Point] = start, directions: List[String] = orthogonal) {
  def proposeMove(elf: Point): Option[Point] =
    if (!allDirections.map(elf.move).exists(elves.contains))
      None
    else
      directions.find(direction => {
        val directionsToCheck = allDirections.filter(_.contains(direction))
        !directionsToCheck.exists(check => elves.contains(elf.move(check)))
      }).map(elf.move)

  def move(elf: Point, proposedMoves: List[Point]): Point =
    proposeMove(elf).filter(x => proposedMoves.count(_ == x) <= 1).getOrElse(elf)

  def next: State = {
    val proposedMoves: List[Point] = elves.toList.flatMap(proposeMove)
    copy(elves = elves.map(elf => move(elf, proposedMoves)), directions = directions.tail ::: List(directions.head))
  }

  def emptyGround: Int = (for(x <- elves.map(_.x).min to elves.map(_.x).max;
                             y <- elves.map(_.y).min to elves.map(_.y).max if !elves.contains(Point(x, y)))
    yield 1).sum
}

val iterations = LazyList.iterate(State())(_.next)
val part1 = iterations(10).emptyGround



