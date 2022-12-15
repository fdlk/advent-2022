case class Point(x: Int, y: Int) {
  def to(other: Point): Point = Point(other.x - x, other.y - y)
  def size: Int = x.abs + y.abs
}

val input = common.loadPackets(List("day15.txt")).map({
  case s"Sensor at x=${sx}, y=${sy}: closest beacon is at x=${bx}, y=${by}" =>
    (Point(sx.toInt, sy.toInt), Point(bx.toInt, by.toInt))
})

def swept(sensor: Point, beacon: Point, line: Int): IndexedSeq[Int] = {
  val distanceToBeacon = sensor.to(beacon).size
  val center = Point(sensor.x, line)
  val distanceToSensor = sensor.to(center).size
  val width = distanceToBeacon - distanceToSensor
  center.x - width to center.x + width
}

val line = 2_000_000

val sweptOnLine = input.map({ case (sensor, beacon) => swept(sensor, beacon, line) })
  .filter(_.nonEmpty)
  .map(_.toSet)
  .reduce(_.union(_))

val beaconsOnLine = input.map(_._2).filter(_.y == line).map(_.x).toSet

val part1 = sweptOnLine.removedAll(beaconsOnLine).size