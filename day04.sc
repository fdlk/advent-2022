import common.loadPackets

def parseRange(description: String): Range = {
  val split = description.split("-")
  Range.inclusive(split(0).toInt, split(1).toInt)
}

val input = loadPackets(List("day04.txt"))
  .map(_.split(",")
    .map(parseRange)
    .map(_.toSet)
    .toList)

val part1 = input.count({
  case List(a, b) => a.subsetOf(b) || b.subsetOf(a)
})

val part2 = input.count({
  case List(a, b) => a.intersect(b).nonEmpty
})