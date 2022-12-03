import common.loadPackets

val input: List[List[String]] = loadPackets(List("day02.txt"))
  .map(_.split("\\s+").toList)

def scoreForShape(shape: String): Int = shape match {
  case "X" => 1
  case "Y" => 2
  case "Z" => 3
}

def scoreForOutcome(round: List[String]) = round match {
  case List("A", "X") => 3
  case List("B", "Y") => 3
  case List("C", "Z") => 3
  case List("A", "Y") => 6
  case List("B", "Z") => 6
  case List("C", "X") => 6
  case _ => 0
}

def part1(round: List[String]) : Int =
  scoreForShape(round(1)) + scoreForOutcome(round)

input.map(part1).sum