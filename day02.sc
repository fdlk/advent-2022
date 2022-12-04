import common.loadPackets

val input: List[List[String]] = loadPackets(List("day02.txt"))
  .map(_.split("\\s+").toList)

sealed abstract class Shape

object Shape {
  case object Rock extends Shape
  case object Paper extends Shape
  case object Scissors extends Shape
  val shapes = Set(Rock, Paper, Scissors)
}

def parseShape(str: String): Shape = str match {
  case "A" | "X" => Shape.Rock
  case "B" | "Y" => Shape.Paper
  case "C" | "Z" => Shape.Scissors
}

def nemesis(shape: Shape): Shape = shape match {
  case Shape.Rock => Shape.Paper
  case Shape.Paper => Shape.Scissors
  case Shape.Scissors => Shape.Rock
}

def scoreForShape(shape: Shape): Int = shape match {
  case Shape.Rock => 1
  case Shape.Paper => 2
  case Shape.Scissors => 3
}

def scoreForOutcome(round: List[Shape]): Int = round match {
  case List(x, y) if x == y => 3
  case List(x, y) if y == nemesis(x) => 6
  case _ => 0
}

def score(round: List[Shape]): Int =
  scoreForShape(round(1)) + scoreForOutcome(round)

val part1 = input.map(_.map(parseShape)).map(score).sum

def parseRound(input: List[String]): List[Shape] = {
  val move = parseShape(input.head)
  val countermove = input(1) match {
    // X means lose
    case "X" => Shape.shapes.find(nemesis(_) == move).get
    // Y means draw
    case "Y" => move
    // Z means win
    case "Z" => nemesis(move)
  }
  List(move, countermove)
}

val part2 = input.map(parseRound).map(score).sum