import common.loadPackets

//val initialState: Map[Int, String] = Map(1 -> "NZ", 2 -> "DCM", 3 -> "P")
/*
[V]     [C]     [M]
[V]     [J]     [N]     [H]     [V]
[R] [F] [N]     [W]     [Z]     [N]
[H] [R] [D]     [Q] [M] [L]     [B]
[B] [C] [H] [V] [R] [C] [G]     [R]
[G] [G] [F] [S] [D] [H] [B] [R] [S]
[D] [N] [S] [D] [H] [G] [J] [J] [G]
[W] [J] [L] [J] [S] [P] [F] [S] [L]
1   2   3   4   5   6   7   8   9
 */
val initialState: Map[Int, String] = Map(
  1 -> "VRHBGDW",
  2 -> "FRCGNJ",
  3 -> "JNDHFSL",
  4 -> "VSDJ",
  5 -> "VNWQRDHS",
  6 -> "MCHGP",
  7 -> "CHZLGBJF",
  8 -> "RJS",
  9 -> "MVNBRSGL"
)

val regex = """move (\d+) from (\d+) to (\d+)""".r

case class Instruction(amount: Int, from: Int, to: Int)

val input: List[Instruction] = loadPackets(List("day05.txt")).map({
  case regex(amount, from, to) => Instruction(amount.toInt, from.toInt, to.toInt)
})

val part1 = input.foldLeft(initialState)((state, instruction) => instruction match {
  case Instruction(amount, from, to) =>
    state.updated(from, state(from).substring(amount))
      .updated(to, state(from).substring(0, amount).reverse + state(to))
}).toList.sortBy(_._1).map(_._2(0)).mkString

val part2 = input.foldLeft(initialState)((state, instruction) => instruction match {
  case Instruction(amount, from, to) =>
    state.updated(from, state(from).substring(amount))
      .updated(to, state(from).substring(0, amount) + state(to))
}).toList.sortBy(_._1).map(_._2(0)).mkString
