import scala.io.Source

object AOC_08_p2 {

    def main(args: Array[String]): Unit = {
        System.out.println(
            visibleTrees(Source.fromResource("day8_input.txt"))
        )
    }

    type Row = Array[Int]
    type Grid = Array[Row]

    case class Coord(x: Int, y:Int)

    def visibleTrees(source: Source): Int = {
        val grid = source.getLines()
          .map(line => line.map(_.toInt - '0').toArray)
          .toArray

        grid.zipWithIndex.map {
            case (row, i) => row.zipWithIndex map {
                case (h, j) => scenicScore(grid,h, Coord(i, j))
            }
        }.map(_.max).max
    }

    def scenicScore(grid: Grid, height: Int, coord: Coord): Int = {
        def isValid(coord: Coord): Boolean =
            coord.x >= 0 && coord.y >= 0 && coord.x < grid(0).length && coord.y < grid.length

        def numTrees(c: Coord, step: Coord => Coord): Int =
            (height, c) match {
                case (_, c) if !isValid(c) => 0
                case (h, c) if h <= grid(c.x)(c.y) => 1
                case (_, c) => 1 + numTrees(step(c), step)
            }

        def calculate(step: Coord => Coord): Int =
            numTrees(step(coord), step)

        Seq(
            calculate(c => Coord(c.x + 1, c.y)),
            calculate(c => Coord(c.x - 1, c.y)),
            calculate(c => Coord(c.x, c.y + 1)),
            calculate(c => Coord(c.x, c.y - 1))
        ).product
    }
}
