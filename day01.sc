import common.loadPackets

val input = loadPackets(List("day01.txt"))

def calories (input: List[String]): List[Int] = input.foldLeft(0, List[Int]())({
  case ((sum: Int, soFar: List[Int]), "") => (0, sum :: soFar)
  case ((sum: Int, soFar: List[Int]), calories) => (sum + calories.toInt, soFar)
})._2

val part1 = calories(input).max

val part2 = calories(input).sorted.reverse.take(3).sum