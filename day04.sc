import common.loadPackets

case class Assignment(from: Int, to: Int) {
  def contains(other: Assignment): Boolean =
    from <= other.from && to >= other.to

  def overlaps(other: Assignment): Boolean =
    to >= other.from && from <= other.to
}

object Assignments {
  def parseAssignment(description: String): Assignment = {
    val split = description.split("-")
    Assignment(split(0).toInt, split(1).toInt)
  }
}

val input:List[List[Assignment]] = loadPackets(List("day04.txt"))
  .map(_.split(",")
    .map(Assignments.parseAssignment)
    .toList)

val part1 = input.count({
  case List(a, b) => a.contains(b) || b.contains(a)
})

val part2 = input.count({
  case List(a, b) => a.overlaps(b)
})