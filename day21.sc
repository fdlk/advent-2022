import reactive._
import scala.annotation.tailrec

val input = common.loadPackets(List("day21.txt")).map(_.split(": ")).map(x => x(0) -> x(1)).toMap
val monkeys = input.keys.map(_ -> Signal(0L)).toMap

val expression = """([a-z]{4}) ([+*/\-]) ([a-z]{4})""".r
input.foreach({
  case (name, expression(left, op, right)) if op == "+" => monkeys(name).update(monkeys(left)() + monkeys(right)())
  case (name, expression(left, op, right)) if op == "*" => monkeys(name).update(monkeys(left)() * monkeys(right)())
  case (name, expression(left, op, right)) if op == "/" => monkeys(name).update(if (monkeys(right)() != 0) monkeys(left)() / monkeys(right)() else 0)
  case (name, expression(left, op, right)) if op == "-" => monkeys(name).update(monkeys(left)() - monkeys(right)())
  case (name, value) => monkeys(name).update(value.toLong)
})

val root = monkeys("root")

val part1 = root()

root.update(input("root") match { case expression(left, _, right) => monkeys(left)() - monkeys(right)() })

def rootMonkey(human: Long): Long = {
  monkeys("humn").update(human)
  root()
}

@tailrec
def binarySearch(from: Long, to: Long): Long = {
  val halfWay = (from + to) / 2
  rootMonkey(from).sign * rootMonkey(halfWay).sign match {
    case 0 => halfWay
    case 1 => binarySearch(halfWay, to)
    case -1 => binarySearch(from, halfWay)
  }
}

val part2 = binarySearch(0, 10000000000000L)