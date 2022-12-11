type Worry = Long
case class Monkey(number: Int,
                  items: List[Worry],
                  operation: Worry => Worry,
                  test: Worry => Int,
                  inspections: Long = 0) {
  def inspectAndThrow: List[(Int, Worry)] = items.map(worry => {
    val newWorry = operation(worry)
    (test(newWorry), newWorry)
  })
}

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

val monkeys = List(
  Monkey(0, List(74, 64, 74, 63, 53), old => old * 7, (x: Worry) => if (x % 5 == 0) 1 else 6),
  Monkey(1, List(69, 99, 95, 62), old => old * old, (x: Worry) => if (x % 17 == 0) 2 else 5),
  Monkey(2, List(59, 81), old => old + 8, (x: Worry) => if (x % 7 == 0) 4 else 3),
  Monkey(3, List(50, 67, 63, 57, 63, 83, 97), old => old + 4, (x: Worry) => if (x % 13 == 0) 0 else 7),
  Monkey(4, List(61, 94, 85, 52, 81, 90, 94, 70), old => old + 3, (x: Worry) => if (x % 19 == 0) 7 else 3),
  Monkey(5, List(69), old => old + 5, (x: Worry) => if (x % 3 == 0) 4 else 2),
  Monkey(6, List(54, 55, 58), old => old + 7, (x: Worry) => if (x % 11 == 0) 1 else 5),
  Monkey(7, List(79, 51, 83, 88, 93, 76), old => old * 3, (x: Worry) => if (x % 2 == 0) 0 else 6),
)

val part1State = State(monkeys.map(monkey => monkey.copy(operation = monkey.operation(_) / 3)))
val part1 = LazyList.iterate(part1State)(_.round)(20).monkeyBusiness

val part2State = State(monkeys.map(monkey => monkey.copy(operation = monkey.operation(_) % (5 * 17 * 7 * 13 * 19 * 3 * 11 * 2))))
val part2 = LazyList.iterate(part2State)(_.round)(10_000).monkeyBusiness