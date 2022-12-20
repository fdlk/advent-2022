val input = common.loadPackets(List("day20.txt")).map(_.toInt).zipWithIndex

def shuffle(list: List[(Int, Int)], element: (Int, Int)): List[(Int, Int)] = {
    val index = list.indexOf(element)
    val newIndex = (index + element._1 + (list.length - 1) * 1000) % (list.length - 1)
    if (newIndex > index) {
      list.slice(0, index) :::
        list.slice(index + 1, newIndex + 1) :::
        List(element) ::: list.slice(newIndex + 1, list.size)
    } else if (newIndex < index) {
      list.slice(0, newIndex) :::
        List(element) ::: list.slice(newIndex, index) :::
        list.slice(index + 1, list.size)
    }
    else list
}

val mixed = input.foldLeft(input)(shuffle).map(_._1)

def repeated: Stream[Int] = mixed.toStream #::: repeated
val part1 = repeated.dropWhile(_ != 0).grouped(1000).map(_.head).take(4).sum