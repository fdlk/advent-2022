import common.loadPackets

val input: List[List[String]] = loadPackets(List("day02.txt"))
  .map(_.split("\\s+").toList)

sealed abstract class Shape

object Shape {
  case object Rock extends Shape

  case object Paper extends Shape

  case object Scissors extends Shape
}

def parseShape(str: String): Shape = str match {
  case "A" => Shape.Rock
  case "B" => Shape.Paper
  case "C" => Shape.Scissors
  case "X" => Shape.Rock
  case "Y" => Shape.Paper
  case "Z" => Shape.Scissors
}

def scoreForShape(shape: Shape): Int = shape match {
  case Shape.Rock => 1
  case Shape.Paper => 2
  case Shape.Scissors => 3
}

def scoreForOutcome(round: List[Shape]): Int = round match {
  case List(x, y) if x == y => 3
  case List(Shape.Paper, Shape.Scissors) => 6
  case List(Shape.Scissors, Shape.Rock) => 6
  case List(Shape.Rock, Shape.Paper) => 6
  case _ => 0
}

def score(round: List[Shape]): Int =
  scoreForShape(round(1)) + scoreForOutcome(round)

val part1 = input.map(_.map(parseShape)).map(score).sum