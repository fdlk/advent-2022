import common.loadPackets

val input = loadPackets(List("day03.txt"))

def overlap(pack: (String, String)): Char =
  pack._1.find(pack._2.contains(_)).get

def priority(item: Char): Int = {
  val lower = 'a' to 'z'
  val upper = 'A' to 'Z'
  if (lower.contains(item))
    lower.indexOf(item) + 1
  else
    upper.indexOf(item) + 27
}

val part1 = input
  .map(pack => pack.splitAt(pack.length / 2))
  .map(overlap)
  .map(priority)
  .sum

def badge(rucksacks: List[String]): Char =
  rucksacks
    .reduce((a, b) => a.filter(b.contains(_)))
    .head

val part2 = input
  .grouped(3)
  .map(badge)
  .map(priority)
  .sum

