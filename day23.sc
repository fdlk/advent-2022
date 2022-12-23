val input = common.loadPackets(List("day23.txt"))

case class Point(row: Int, col: Int) {
  def move(direction: String): Point = direction.toList match {
    case List('N') => copy(row = row - 1)
    case List('S') => copy(row = row + 1)
    case List('E') => copy(col = col + 1)
    case List('W') => copy(col = col - 1)
    case List(a, b) => move(a.toString).move(b.toString)
  }
}

val allDirections = List("NW", "N", "NE", "E", "SE", "S", "SW", "W")
val start: Set[Point] = (for (row <- input.indices;
                              col <- input(row).indices if input(row)(col) == '#')
yield Point(row, col)).toSet

case class State(elves: Set[Point] = start, directions: List[String] = List("N", "S", "W", "E")) {
  val proposedMoves: List[Point] = elves.toList.flatMap(proposeMove)
  def proposeMove(elf: Point): Option[Point] =
    if (!allDirections.map(elf.move).exists(elves.contains))
      None
    else
      directions
        .find(direction => !allDirections.filter(_.contains(direction)).map(elf.move).exists(elves.contains))
        .map(elf.move)

  def move(elf: Point): Point = proposeMove(elf).filter(x => proposedMoves.count(_ == x) == 1).getOrElse(elf)

  def next: State = copy(elves = elves.map(move), directions = directions.tail ::: List(directions.head))

  def span(values: Set[Int]): Int = values.max - values.min + 1

  def emptyGround: Int = span(elves.map(_.row)) * span(elves.map(_.col)) - elves.size
}

val iterations = LazyList.iterate(State())(_.next)
val part1 = iterations(10).emptyGround

val part2 = iterations.sliding(2).map(_.toList)
  .indexWhere({ case List(before, after) => before.elves == after.elves }) + 1