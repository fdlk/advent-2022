import common.loadPackets

val input = loadPackets(List("day08.txt"))
  .map(_.toList.map(c => Integer.parseInt(c.toString)))

case class Point(x: Int, y: Int) {
  val isWithinBounds: Boolean =
    input.indices.contains(y) && input.head.indices.contains(x)
  lazy val height: Int = input(y)(x)
  def plus(delta: Point, steps: Int = 1): Point =
    copy(x = x + delta.x * steps, y = y + delta.y * steps)
}

val forest = for(
  y <- input.indices;
  x <- input.head.indices
) yield Point(x, y)

val directions: List[Point] = List(Point(-1, 0), Point(1, 0), Point(0, 1), Point(0, -1))

def isVisible(tree: Point, direction: Point): Boolean =
  LazyList.from(1)
    .map(tree.plus(direction, _))
    .takeWhile(_.isWithinBounds)
    .forall(_.height < tree.height)

val part1 = forest.count(tree => directions.exists(isVisible(tree, _)))

def amountOfTreesVisibleInDirection(tree: Point, direction: Point, steps: Int = 1): Int = {
  val nextTree = tree.plus(direction, steps)
  if (!nextTree.isWithinBounds) 0
  else if (nextTree.height >= tree.height) 1
  else 1 + amountOfTreesVisibleInDirection(tree, direction, steps + 1)
}

def amountOfTreesVisible(tree: Point): Int =
  directions.map(amountOfTreesVisibleInDirection(tree, _)).product

val part2 = forest.map(amountOfTreesVisible).max