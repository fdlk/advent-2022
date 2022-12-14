import scala.annotation.tailrec

case class Point(x: Int, y: Int) {
  def diff(to: Point) = Point(to.x - x, to.y - y)

  def sign: Point = Point(x.sign, y.sign)

  def plus(direction: Point, step: Int = 1) = Point(x + step * direction.x, y + step * direction.y)

  def size: Int = x.abs + y.abs
}

object Points {
  def of(s: String): Point = s match {
    case s"${x},${y}" => Point(x.toInt, y.toInt)
  }

  def line(from: Point, to: Point): Set[Point] = {
    val diff = from.diff(to)
    (0 to diff.size).map(from.plus(diff.sign, _)).toSet
  }
}


val input = common.loadPackets(List("day14.txt"))
  .flatMap(_.split(" -> ")
    .map(Points.of)
    .sliding(2)
    .flatMap({ case Array(from, to) => Points.line(from, to) }))
  .toSet

case class State(rock: Set[Point] = input, sand: Set[Point] = Set()) {
  val directions = List(Point(0, 1), Point(-1, 1), Point(1, 1))

  def isFree(p: Point): Boolean = !rock.contains(p) && !sand.contains(p)

  def offTheMap(p: Point): Boolean = p.y > rock.map(_.y).max

  @tailrec
  final def landingSite(x: Point): Option[Point] =
    directions.map(x.plus(_)).find(isFree) match {
      case None => Some(x)
      case Some(lost) if offTheMap(lost) => None
      case Some(next) => landingSite(next)
    }

  val fount: Point = Point(500, 0)
  def next: Option[State] = landingSite(fount).map(value => copy(sand = sand + value))
}
val start: Option[State] = Some(State())
Iterator.iterate(start)(_.flatMap(_.next)).takeWhile(_.nonEmpty).toList.last.get.sand.size

