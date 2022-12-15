case class Point(x: Int, y: Int) {
  def to(other: Point): Point = Point(other.x - x, other.y - y)
  def size: Int = x.abs + y.abs
}

case class Interval(from: Int, to: Int) {
  val empty: Boolean = to < from
  val size: Int = to - from + 1
  def touches(o: Interval): Boolean = from <= o.to && o.from <= to
  def restrict(o: Interval): Interval = copy(from = from.max(o.from), to = to.min(o.to))
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

def add(swept: Set[Interval], o: Interval): Set[Interval] = {
  val (touching, disjoint) = (swept + o).partition(o.touches)
  disjoint + Interval(touching.map(_.from).min, touching.map(_.to).max)
}

val line = 2_000_000

val sweptOnLine = input.map({ case (sensor, beacon) => swept(sensor, beacon, line) })
  .filterNot(_.empty)
  .foldLeft(Set[Interval]())(add)
val beaconsOnLine = input.map(_._2).count(_.y == line)

val part1 = sweptOnLine.toList.map(_.size).sum - beaconsOnLine

val max = 4_000_000
val searchBox = Interval(0, max)

def sweepLine(y: Int): Option[Point] = {
  val sweptOnLine = input.map({ case (sensor, beacon) => swept(sensor, beacon, y) })
    .map(_.restrict(searchBox))
    .filterNot(_.empty)
    .foldLeft(Set[Interval]())(add)
  if (sweptOnLine.contains(searchBox)) None
  else Some(Point(sweptOnLine.minBy(_.from).to + 1, y))
}

val part2 = (0 to max).flatMap(sweepLine)
  .map(p => p.x * 4000000L + p.y).head
