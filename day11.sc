type Worry = Int

case class Monkey(items: List[Worry],
                  operation: Worry => Worry,
                  divisor: Int, targetIfTrue: Int, targetIfFalse: Int,
                  inspections: Long = 0) {
  def inspectItems: (List[Worry], List[Worry]) = items.map(operation).partition(_ % divisor == 0)
  def done: Monkey = copy(items = Nil, inspections = inspections + items.size)
  def catchItems(caught: List[Worry]): Monkey = copy(items = items ::: caught)
}

object Monkeys {
  def parse(lines: List[String]): Monkey = Monkey(
    items = lines(1) match { case s"  Starting items: ${items}" => items.split(", ").map(_.toInt).toList },
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
    val monkey = monkeys(index)
    val (yes, no) = monkey.inspectItems
    copy(monkeys = monkeys
      .updated(index, monkeys(index).done)
      .updated(monkey.targetIfTrue, monkeys(monkey.targetIfTrue).catchItems(yes))
      .updated(monkey.targetIfFalse, monkeys(monkey.targetIfFalse).catchItems(no)))
  }
  def round: State = monkeys.indices.foldLeft(this)((state, index) => state.processMonkey(index))
  lazy val monkeyBusiness = monkeys.map(_.inspections).sorted.reverse.take(2).product
}

val part1State = State(input.map(monkey => monkey.copy(operation = monkey.operation(_) / 3)))
val part1 = Iterator.iterate(part1State)(_.round).drop(20).next.monkeyBusiness

val modulo = input.map(_.divisor).product
val part2State = State(input.map(monkey => monkey.copy(operation = monkey.operation(_) % modulo)))
val part2 = Iterator.iterate(part2State)(_.round).drop(10_000).next.monkeyBusiness