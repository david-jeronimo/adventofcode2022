import scala.annotation.tailrec
import scala.io.Source
import scala.math.abs

object AOC_12 {

    def main(args: Array[String]): Unit = {
        Seq(
            climbSteps(Source.fromResource("day12_sample.txt"), PartOne),
            climbSteps(Source.fromResource("day12_input.txt"), PartOne),
            climbSteps(Source.fromResource("day12_sample.txt"), PartTwo),
            climbSteps(Source.fromResource("day12_input.txt"), PartTwo),
        ).foreach(println)
    }

    case class Position(x: Int, y: Int)
    type Row = Array[Int]
    type Grid = Array[Row]
    case class Input(grid: Grid, start: Option[Position], end: Option[Position])
    case class State(position: Position, height: Int)

    trait Part
    case object PartOne extends Part
    case object PartTwo extends Part

    type ValidMovement = (Grid, Position, Position) => Boolean

    def climbSteps(source: Source, part: Part): Int = {
        val input = source.getLines().zipWithIndex
          .foldLeft(Input(Array.empty: Grid, None: Option[Position], None: Option[Position]))(lineToRow)

        part match {
            case PartOne => search(
                input.grid,
                Seq(State(input.start.get, 0)),
                0,
                isSolution = _.position == input.end.get,
                isValidMovement = (grid, p1, p2) => grid(p2.y)(p2.x) - grid(p1.y)(p1.x) <= 1,
                priority = s => (-s.height, distance(s.position, input.end.get))
            )
            case PartTwo => search(
                input.grid,
                Seq(State(input.end.get, 'z' - 'a')),
                0,
                isSolution = _.height == 0,
                isValidMovement = (grid, p1, p2) => grid(p1.y)(p1.x) - grid(p2.y)(p2.x) <= 1,
                priority = s => (s.height, -distance(s.position, input.end.get))
            )
        }
    }

    def lineToRow(input: Input, line: (String, Int)): Input = {
        val lineInput = line._1.foldLeft(
            (Array.empty: Row, input.start, input.end)
        )((state, c) => (state, c) match {
            case ((row, _, end), 'S') => (row :+ 0, Some(Position(row.length, line._2)), end)
            case ((row, start, _), 'E') => (row :+ ('z' - 'a'), start , Some(Position(row.length, line._2)))
            case ((row, start, end), c) => (row :+ (c - 'a'), start, end)
        })
        Input(input.grid :+ lineInput._1, lineInput._2, lineInput._3)
    }

    def distance(p1: Position, p2: Position): Int =
        abs(p1.x - p2.x) + abs(p1.y - p2.y)

    @tailrec
    def search(grid: Grid, states: Seq[State], count: Int, isSolution: State => Boolean,
               isValidMovement: ValidMovement, priority: State => (Int, Int)): Int =
        states.groupBy(_.position).values
          .map(g => g.head).toSeq
          .sortBy(priority(_)) match {
            case _ if states.exists(isSolution(_)) => count
            case s => search(grid, s.flatMap(climb(grid, _, isValidMovement)), count + 1, isSolution, isValidMovement, priority)
        }

    def climb(grid: Grid, state: State, isValidMovement: ValidMovement): Seq[State] =
        nextPositions(grid, state.position, isValidMovement)
          .map(nextPosition => State(nextPosition, grid(nextPosition.y)(nextPosition.x)))

    def nextPositions(grid: Grid, position: Position, isValidMovement: ValidMovement): Seq[Position] =
        Seq[Position => Position](
            p => Position(p.x + 1, p.y),
            p => Position(p.x - 1, p.y),
            p => Position(p.x, p.y + 1),
            p => Position(p.x, p.y - 1)
        ).map(_ (position))
          .filter(p => p.x >= 0 && p.y >= 0 && p.x < grid(0).length && p.y < grid.length && isValidMovement(grid, position, p))
}
