val toDigit = Map('=' -> -2, '-' -> -1, '0' -> 0, '1' -> 1, '2' -> 2)
val toSnafu = toDigit.map(_.swap)

val input = common.loadPackets(List("day25.txt"))
val snafus = input.map(_.map(toDigit).reverse.toList)

type Snafu = List[Int]

object Snafus {
  def add(s1: Snafu, s2: Snafu, carry: Int = 0): Snafu = (s1.headOption, s2.headOption) match {
    case (None, None) if carry == 0 => Nil
    case (None, None) => List(carry)
    case (Some(h), None) => addTails(h + carry, s1.tail, Nil)
    case (None, Some(h)) => addTails(h + carry, s2.tail, Nil)
    case (Some(h1), Some(h2)) => addTails(h1 + h2 + carry, s1.tail, s2.tail)
  }

  def addTails(sum: Int, t1: Snafu, t2: Snafu): Snafu =
    Math.floorMod(sum + 2, 5) - 2 ::
      add(t1, t2, if (sum > 2) 1 else if (sum < -2) -1 else 0)
}

snafus.reduce(Snafus.add(_, _)).map(toSnafu).reverse.mkString