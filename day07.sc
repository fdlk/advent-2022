import common.loadPackets

val input = loadPackets(List("day07.txt"))

case class State(cwd : List[String] = List("/"), sizes: Map[List[String], Int] = Map()) {
  def withFile(size: Int, dir: List[String] = cwd): State = dir match {
    case Nil => this
    case _ => copy(sizes = sizes.updated(dir, sizes.getOrElse(dir, 0) + size))
      .withFile(size, dir.tail)
  }
}

val dir = """dir (.*)""".r
val file = """(\d+) (.*)""".r
val cd = """\$ cd (.*)""".r

def reduce(state: State, line: String): State = line match {
  case "$ cd /" => state.copy(cwd = List("/"))
  case "$ ls" => state
  case dir(_) => state
  case file(size, _) => state.withFile(size.toInt)
  case "$ cd .." => state.copy(cwd = state.cwd.tail)
  case cd(dir) => state.copy(cwd = dir :: state.cwd)
}

val sizes = input.foldLeft(State())(reduce).sizes.values
val part1 = sizes.filter(_ <= 100_000).sum

val totalSpaceUsed = sizes.max
val totalSpaceLeft = 70_000_000 - totalSpaceUsed
val toBeDeleted = 30_000_000 - totalSpaceLeft

val part2 = sizes.filter(_ >= toBeDeleted).min
