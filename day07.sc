import common.loadPackets

val input = loadPackets(List("day07.txt"))

case class State(cwd : List[String] = List("/"), sizes: Map[List[String], Int] = Map()) {
  def withFile(size: Int, dir: List[String] = cwd): State = dir match {
    case Nil => this
    case _ => copy(sizes = sizes.updated(dir, sizes.getOrElse(dir, 0) + size))
      .withFile(size, dir.tail)
  }
}

def reduce(state: State, line: String): State = line match {
  case "$ cd /" => state.copy(cwd = List("/"))
  case "$ ls" => state
  case s"dir ${_}" => state
  case "$ cd .." => state.copy(cwd = state.cwd.tail)
  case s"$$ cd ${dir}" => state.copy(cwd = dir :: state.cwd)
  case s"${size} ${_}" => state.withFile(size.toInt)
}

val sizes = input.foldLeft(State())(reduce).sizes.values
val part1 = sizes.filter(_ <= 100_000).sum

val totalSpaceUsed = sizes.max
val totalSpaceLeft = 70_000_000 - totalSpaceUsed
val toBeDeleted = 30_000_000 - totalSpaceLeft

val part2 = sizes.filter(_ >= toBeDeleted).min
