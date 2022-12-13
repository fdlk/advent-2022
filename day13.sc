import fastparse._
import common._

val input = loadPackets(List("day13.txt"))

def parseExpression(line: String): Any = {
  val Parsed.Success(value, _) = parse(line, common.expr(_))
  value
}

def isInOrder(pair: (Any, Any)): Option[Boolean] = pair match {
  case (a: Int, b: Int) if a < b => Some(true)
  case (a: Int, b: Int) if a == b => None
  case (a: Int, b: Int) if a > b => Some(false)
  case (a: List[Any], b: List[Any]) => (a, b) match {
    case (a :: _, b :: _) if isInOrder(a, b).nonEmpty => isInOrder((a, b))
    case (_ :: as, _ :: bs) => isInOrder((as, bs))
    case (Nil, Nil) => None
    case (Nil, _) => Some(true)
    case (_, Nil) => Some(false)
  }
  case (a: Int, b: List[Any]) => isInOrder((List(a), b))
  case (a: List[Any], b: Int) => isInOrder((a, List(b)))
}

val part1 = input.grouped(3)
  .map(lines => (parseExpression(lines.head), parseExpression(lines(1))))
  .map(pair => isInOrder(pair).get)
  .zipWithIndex
  .filter(_._1)
  .map(_._2 + 1)
  .sum

val divider1 = parseExpression("[[2]]")
val divider2 = parseExpression("[[6]]")
val sorted = (divider1 :: divider2 :: input.filter(_.nonEmpty).map(parseExpression))
  .sortWith((a, b) => isInOrder(a, b).get)

val part2 = (sorted.indexOf(divider1) + 1) * (sorted.indexOf(divider2) + 1)