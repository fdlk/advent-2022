import common.loadPackets

val input = loadPackets(List("day03.txt"))

def overlap(pack: (String, String)): Char =
  pack._1.find(pack._2.contains(_)).get

def priority(item: Char): Int =
  if (item >= 'a') item - 'a' + 1
  else item - 'A' + 27

val part1 = input
  .map(pack => pack.splitAt(pack.length / 2))
  .map(overlap)
  .map(priority)
  .sum

