import common.loadPackets

val input = loadPackets(List("day08.txt"))
  .map(_.toList.map(c => Integer.parseInt(c.toString)))

val height = input.length
val width = input.head.length

case class Point(x: Int, y: Int) {
  def plus(delta: Point, steps: Int = 1): Point =
    copy(x = x + delta.x * steps, y = y + delta.y * steps)

  def isWithinBounds: Boolean =
    input.indices.contains(y) && input.head.indices.contains(x)

  def height: Int =
    input(y)(x)
}



def isVisible(tree: Point): Boolean = {
  (0 until tree.x).forall(x => input(tree.y)(x) < tree.height) ||
    (tree.x + 1 until width).forall(x => input(tree.y)(x) < tree.height) ||
    (0 until tree.y).forall(y => input(y)(tree.x) < tree.height) ||
    (tree.y + 1 until height).forall(y => input(y)(tree.x) < tree.height)
}

val part1 = (for(y <- 0 until height;
    x <- 0 until width if isVisible(Point(x, y)))
yield (x, y)).size

def amountOfTreesVisibleInDirection(tree: Point, delta: Point, steps: Int = 1): Int = {
  val nextTree = tree.plus(delta, steps)
  if (!nextTree.isWithinBounds) 0
  else if (nextTree.height >= tree.height) 1
  else 1 + amountOfTreesVisibleInDirection(tree, delta, steps + 1)
}

def amountOfTreesVisible(tree: Point): Int =
  amountOfTreesVisibleInDirection(tree, Point(-1, 0)) *
    amountOfTreesVisibleInDirection(tree, Point(1, 0)) *
    amountOfTreesVisibleInDirection(tree, Point(0, 1)) *
    amountOfTreesVisibleInDirection(tree, Point(0, -1))

val part2 = (for(y <- 0 until height;
                 x <- 0 until width)
yield amountOfTreesVisible(Point(x, y))).max