import scala.annotation.tailrec

val input = common.loadPackets(List("day25.txt"))

def parseDigit(digit: Char): Int = digit match {
  case '=' => -2
  case '-' => -1
  case _ => digit - '0'
}

def toDigit(digit: Int): Char = digit match {
  case -2 => '='
  case -1 => '-'
  case _ => ('0' + digit).toChar
}

def parseSnafu(value: String): Long = value.reverse.zipWithIndex.map({
  case (digit, power) => parseDigit(digit) * Math.pow(5, power).toLong
}).sum

val totalFuel = input.map(parseSnafu).sum

@tailrec
def numDigitsNeeded(value: Long, twos: String = ""): Int =
  if (parseSnafu(twos) >= value)
    twos.length
  else
    numDigitsNeeded(value, twos + "2")

def toSnafu(value: Long): String = {
  val digits = numDigitsNeeded(value)
  val diff: BigInt = value - parseSnafu(LazyList.fill(digits)('=').mkString)
  diff.toString(5).map(parseDigit).map(_ - 2).map(toDigit).mkString
}

val part1 = toSnafu(totalFuel)