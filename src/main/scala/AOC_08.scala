import scala.io.Source

object AOC_08 {

    def main(args: Array[String]): Unit = {
        System.out.println(
            visibleTrees(Source.fromResource("day8_sample.txt"))
        )
    }

    type Row = Array[Int]
    type Grid = Array[Row]
    type Hidden = Array[Array[Boolean]]

    def visibleTrees(source: Source): Int = {
        val grid = source.getLines()
          .map(line => line.map(_.toInt - '0').toArray)
          .toArray
        val hidden: Hidden = Array.fill(grid.length)(Array.fill(grid(0).length)(true))
        Seq[Hidden => Hidden](
            traverseH(grid, _, Seq.fill(grid.length)(-1)),
            traverseH(grid, _, Seq.fill(grid.length)(-1), true),
            traverseV(grid, _, 0, Seq.fill(grid.length)(-1), _ + 1, grid.length),
            traverseV(grid, _, grid.length - 1, Seq.fill(grid(0).length)(-1), _ - 1, -1).reverse
        ).foldLeft(hidden)((a, f) => f(a))
          .map(r => r.count(_ == false))
          .sum
    }

    def traverseH(grid: Grid, hidden: Hidden, maxHeights: Seq[Int], reverse: Boolean = false): Hidden =
        grid.zipWithIndex.map {
            case (row, i) if reverse => traverseRow(row, hidden(i), maxHeights(i), _ - 1, row.length - 1, 0).reverse.toArray
            case (row, i) => traverseRow(row, hidden(i), maxHeights(i), _ + 1, 0, row.length - 1).toArray
        }

    def traverseRow(row: Row, hiddenRow: Seq[Boolean], maxHeight: Int, step: Int => Int, from: Int, to: Int): Seq[Boolean] =
        (from, row(from), maxHeight) match {
            case (_, h, mh) if from == to => Seq(h <= mh && hiddenRow(from))
            case (_, h, mh) => (h <= mh && hiddenRow(from)) +: traverseRow(row, hiddenRow, math.max(h, mh), step, step(from), to)
        }

    def traverseV(grid: Grid, hidden: Hidden, i: Int, maxHeights: Seq[Int], step: Int => Int, until: Int): Hidden =
        i match {
            case i if i == until => Array()
            case _ =>
                grid(i).zipWithIndex.map {
                    case (h, j) => hidden(i)(j) && (h <= maxHeights(j))
                } +: traverseV(grid, hidden, step(i), maxHeights.zip(grid(i)).map(a => math.max(a._1, a._2)), step, until)
        }
}
