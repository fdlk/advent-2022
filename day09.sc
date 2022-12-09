import common.loadPackets

case class Point(x: Int = 0, y: Int = 0) {
  def move (direction: Point): Point =
    copy(x = x + direction.x, y = y + direction.y)
  def diff (other: Point): Point = Point(other.x - x, other.y - y)
  def signum: Point = Point(x.sign, y.sign)
  def touching (other: Point): Boolean = diff(other) match {
    case Point(x, y) => x.abs <= 1 && y.abs <= 1
  }
}

val input = loadPackets(List("day09.txt"))
  .map(_.split(" "))
  .flatMap({ case Array(c, count) => List.tabulate(count.toInt)(_ => c.charAt(0))})
  .map {
    case 'R' => Point(1, 0)
    case 'L' => Point(-1, 0)
    case 'U' => Point(0, 1)
    case 'D' => Point(0, -1)
  }

type Rope = List[Point]

def move(rope: Rope, direction: Point): Rope =
  rope.tail.scanLeft(rope.head.move(direction))((h, t) =>
    if (h.touching(t)) t
    else t.move(t.diff(h).signum)
  )

def track(start: Rope): Int = input.scanLeft(start)(move).map(_.last).toSet.size

val part1 = track(List.tabulate(2)(_ => Point()))
val part2 = track(List.tabulate(10)(_ => Point()))