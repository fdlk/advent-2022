import fastparse.{parse, Parsed}
import common.{loadPackets, expr}

def parseExpression(line: String): Any = parse(line, expr(_)) match {
  case Parsed.Success(value, _) => value
}

val input: List[Any] = loadPackets(List("day13.txt")).filter(_.nonEmpty).map(parseExpression)

def compare(a: Any, b: Any): Int = (a, b) match {
  case (a: Int, b: Int) => a.compare(b)
  case (a :: _, b :: _) if compare(a, b) != 0 => compare(a, b)
  case (_ :: as, _ :: bs) => compare(as, bs)
  case (Nil, Nil) => 0
  case (Nil, _) => -1
  case (_, Nil) => 1
  case (a: Int, b: List[Any]) => compare(List(a), b)
  case (a: List[Any], b: Int) => compare(a, List(b))
}

val part1 = input.grouped(2)
  .map({ case List(a, b) => compare(a, b) })
  .zipWithIndex
  .filter(_._1 < 0)
  .map(_._2 + 1)
  .sum

val dividers = List("[[2]]","[[6]]").map(parseExpression)
val sorted = (dividers ::: input).sorted(compare)
val part2 = dividers.map(sorted.indexOf).map(_ + 1).product