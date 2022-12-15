case class Point(x: Int, y: Int) {
  def to(other: Point): Point = Point(other.x - x, other.y - y)
  def size: Int = x.abs + y.abs
}

case class Interval(from: Int, to: Int) {
  val empty: Boolean = to < from

  def contains(o: Interval): Boolean = o.from >= from && o.to <= to
  def disjunct(o: Interval): Boolean = o.from > to || o.to < from
  def subtract(o: Interval): List[Interval] = o match {
    case _ if disjunct(o) => List(this)
    case _ if o.contains(this) => Nil
    case _ if contains(o) =>
      List(Interval(from, o.from - 1), Interval(o.to + 1, to)).filterNot(_.empty)
    case _ if o.from > from => List(Interval(from, o.from - 1)).filterNot(_.empty)
    case _ => List(Interval(o.to + 1, to)).filterNot(_.empty)
  }
}

val input = common.loadPackets(List("day15.txt")).map({
  case s"Sensor at x=${sx}, y=${sy}: closest beacon is at x=${bx}, y=${by}" =>
    (Point(sx.toInt, sy.toInt), Point(bx.toInt, by.toInt))
})

def swept(sensor: Point, beacon: Point, line: Int): Interval = {
  val distanceToBeacon = sensor.to(beacon).size
  val center = Point(sensor.x, line)
  val distanceToSensor = sensor.to(center).size
  val width = distanceToBeacon - distanceToSensor
  Interval(center.x - width, center.x + width)
}

val line = 20000
val sweptOnLine = input.map({ case (sensor, beacon) => swept(sensor, beacon, line) })
  .filterNot(_.empty)
  .map({case Interval(from, to) => (from to to).toSet})
  .reduce(_.union(_))
val beaconsOnLine = input.map(_._2).filter(_.y == line).map(_.x).toSet

val part1 = sweptOnLine.removedAll(beaconsOnLine).size

val max = 4_000_000
def sweepLine(y: Int): Option[Point] = {
  val intervals = input.map({ case (sensor, beacon) => swept(sensor, beacon, y) })
    .filterNot(_.empty)
    .foldLeft(Set(Interval(0, max)))((stillPossible, swept) => stillPossible.flatMap(_.subtract(swept)))
  if (intervals.isEmpty) None else Some(Point(intervals.head.from, y))
}

val part2 = (0 to max).flatMap(sweepLine).map(p => p.x * 4000000L + p.y).head
