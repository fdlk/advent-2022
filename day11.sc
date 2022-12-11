type Worry = Long

case class Monkey(number: Int,
                  items: List[Worry],
                  operation: Worry => Worry,
                  divisor: Int, targetIfTrue: Int, targetIfFalse: Int,
                  inspections: Long = 0) {
  def inspectAndThrow: List[(Int, Worry)] = items.map(worry => {
    val newWorry = operation(worry)
    (if (newWorry % divisor == 0) targetIfTrue else targetIfFalse, newWorry)
  })
}

object Monkeys {
  def parse(lines: List[String]): Monkey = Monkey(
    number = lines.head match { case s"Monkey ${index}:" => index.toInt },
    items = lines(1) match { case s"  Starting items: ${items}" => items.split(", ").map(_.toLong).toList },
    operation = lines(2) match {
      case s"  Operation: new = old * old" => old => old * old
      case s"  Operation: new = old * ${factor}" => _ * factor.toInt
      case s"  Operation: new = old + ${term}" => _ + term.toInt
    },
    divisor = lines(3) match { case s"  Test: divisible by ${divisor}" => divisor.toInt },
    targetIfTrue = lines(4) match { case s"    If true: throw to monkey ${target}" => target.toInt },
    targetIfFalse = lines(5) match { case s"    If false: throw to monkey ${target}" => target.toInt })
}

val input: List[Monkey] = common.loadPackets(List("day11.txt")).grouped(7).map(Monkeys.parse).toList

case class State(monkeys: List[Monkey]) {
  def processMonkey(index: Int): State = {
    val thrownItems = monkeys(index).inspectAndThrow
    copy(monkeys = monkeys.map(monkey =>
      if (monkey.number == index)
        monkey.copy(items = Nil, inspections = monkey.inspections + thrownItems.size)
      else
        monkey.copy(items = monkey.items ::: thrownItems.filter(_._1 == monkey.number).map(_._2))
    ))
  }
  def round: State = monkeys.indices.foldLeft(this)((state, index) => state.processMonkey(index))
  val monkeyBusiness = monkeys.map(_.inspections).sorted.reverse.take(2).product
}

val part1State = State(input.map(monkey => monkey.copy(operation = monkey.operation(_) / 3)))
val part1 = LazyList.iterate(part1State)(_.round)(20).monkeyBusiness

val modulo = input.map(_.divisor).product
val part2State = State(input.map(monkey => monkey.copy(operation = monkey.operation(_) % modulo)))
val part2 = LazyList.iterate(part2State)(_.round)(10_000).monkeyBusiness