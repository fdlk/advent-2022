import common.loadPackets

val input = loadPackets(List("day06.txt")).head

val part1 = input.toSeq.sliding(4).indexWhere(_.toSet.size == 4) + 4
val part2 = input.toSeq.sliding(14).indexWhere(_.toSet.size == 14) + 14