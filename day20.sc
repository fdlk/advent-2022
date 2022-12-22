val input = common.loadPackets(List("day20.txt")).map(_.toInt)

def shuffle(list: List[(Long, Int)], element: (Long, Int)): List[(Long, Int)] = {
  val index = list.indexOf(element)
  val newIndex = Math.floorMod(index + element._1, list.length - 1)
  if (newIndex > index) {
    list.slice(0, index) :::
      list.slice(index + 1, newIndex + 1) :::
      List(element) :::
      list.slice(newIndex + 1, list.size)
  } else if (newIndex < index) {
    list.slice(0, newIndex) :::
      List(element) :::
      list.slice(newIndex, index) :::
      list.slice(index + 1, list.size)
  }
  else list
}
def decrypt(key: Long, mixAmount: Int): Long = {
  val inputMultiplied = input.map(_ * key).zipWithIndex
  val mixed = LazyList.iterate(inputMultiplied)(inputMultiplied.foldLeft(_)(shuffle))(mixAmount).map(_._1)
  def repeat: LazyList[Long] = LazyList.from(mixed) #::: repeat
  repeat.dropWhile(_ != 0).grouped(1000).map(_.head).slice(1, 4).sum
}

val part1 = decrypt(1, 1)
val part2 = decrypt(811589153L, 10)