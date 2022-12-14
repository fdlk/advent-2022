import common.loadPackets

val input = loadPackets(List("day10.txt")).flatMap({
  case s"addx ${value}" => List(0, value.toInt)
  case "noop" => List(0)
})

val signal = input.scanLeft(1)(_ + _)

val part1 = (20 to 220 by 40)
  .map(index => index * signal(index - 1)).sum

val part2 = signal.zipWithIndex
  .map({ case (a, b) => if(Set(a - 1, a, a + 1).contains(b % 40)) '█' else ' '})
  .grouped(40).map(_.mkString).mkString("\n")

/*
████  ██  █     ██  █  █ █    ███   ██
█    █  █ █    █  █ █  █ █    █  █ █  █
███  █  █ █    █    █  █ █    █  █ █
█    ████ █    █ ██ █  █ █    ███  █ ██
█    █  █ █    █  █ █  █ █    █    █  █
████ █  █ ████  ███  ██  ████ █     ███
*/