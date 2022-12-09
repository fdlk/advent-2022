import common.loadPackets

val input = loadPackets(List("day09.txt"))
  .map(_.split(" "))
  .flatMap({ case Array(c, count) => List.tabulate(count.toInt)(_ => c.charAt(0))})

case class Point(x: Int = 0, y: Int = 0) {
  def move (direction: Point): Point =
    copy(x = x + direction.x, y = y + direction.y)
  def diff (other: Point): Point = Point(other.x - x, other.y - y)
  def signum: Point = Point(Math.signum(x).toInt, Math.signum(y).toInt)
  def touching (other: Point): Boolean = diff(other) match {
    case Point(x, y) => x.abs <= 1 && y.abs <= 1
  }
}
def followTail(head: Point, tail: List[Point]): List[Point] = tail match {
  case Nil => Nil
  case p :: _ if p.touching(head) => tail
  case p :: rest =>
    val m = p.move(p.diff(head).signum)
    m :: followTail(m, rest)
}

case class Rope(head: Point = Point(), tail: List[Point] = List(Point())) {
  def moveHead(c: Char): Rope = c match {
    case 'R' => copy(head = head.move(Point(1, 0)))
    case 'L' => copy(head = head.move(Point(-1, 0)))
    case 'U' => copy(head = head.move(Point(0, 1)))
    case 'D' => copy(head = head.move(Point(0, -1)))
  }
  def follow: Rope = copy(tail = followTail(head, tail))
}

def track(start: Rope): Int = input.scanLeft(start)((rope: Rope, move: Char) => rope.moveHead(move).follow)
  .map(_.tail.last).toSet.size

val part1 = track(Rope())
val part2 = track(Rope(tail = List.tabulate(9)(_ => Point())))