import scala.annotation.tailrec
import scala.io.Source

object AOC_17 {

    def main(args: Array[String]): Unit = {
        Seq(
            towerHeight(Source.fromResource("day17_sample.txt"), 2022),
            towerHeight(Source.fromResource("day17_input.txt"), 2022),
        ).foreach(println)
        towerHeightPartTwo(Source.fromResource("day17_sample.txt"))
        towerHeightPartTwo(Source.fromResource("day17_input.txt"))
    }

    sealed trait Direction { def move: (Shape, Chamber) => Either[Shape, Shape]}
    case object L extends Direction{ val move = translate(-1, 0)}
    case object R extends Direction{ val move = translate(1, 0)}
    case object Down extends Direction{ val move = translate(0, 1)}

    sealed trait Cell
    case object Rock extends Cell
    case object Air extends Cell
    type Column = List[Cell]
    type Chamber = List[Column]

    case class Position(x: Int, y: Int)
    type Shape = List[Position]

    case class State(chamber: Chamber, rockCount: Int, movementCount: Int)

    case class Directions(directions: List[Direction])

    def towerHeight(source: Source, numRocks: Long): Int = {
        val directions = source.getLines().map(lineToDirections).toList.head
        val chamber = List.fill(7)(List(Rock))
        buildTower(State(chamber, 0, 0), directions, numRocks)
          .chamber.head.length - 1
    }

    def towerHeightPartTwo(source: Source): Unit = {
        val directions = source.getLines().map(lineToDirections).toList.head
        val chamber = List.fill(7)(List(Rock))
        val cycle = (cycleLength(State(chamber, 0, 0), directions, List()).rockCount - directions.length) / 2
        val fullCycles: Double = math.floor(1000000000000D / cycle)
        val repetitionHeight1: Long = buildTower(State(chamber, 0, 0), directions, 2 * cycle)
          .chamber.head.length - 1
        val repetitionHeight2: Long = buildTower(State(chamber, 0, 0), directions, 3 * cycle)
          .chamber.head.length - 1
        val cycleHeight: Long = repetitionHeight2 - repetitionHeight1
        val fullCyclesHeight: Double = fullCycles * cycleHeight + 1
        val fmt = new java.text.DecimalFormat("####0.##############")
        val remainingCycles: Double = math.floor(1000000000000D % cycle)
        val remainingHeight = buildTower(State(chamber, 0, 0), directions, remainingCycles.toInt)
          .chamber.head.length - 1

        println(fmt.format(fullCyclesHeight + remainingHeight - 1))

    }

    def lineToDirections(line: String): List[Direction] =
        line.map{
            case '<' => L
            case '>' => R
        }.toList

    @tailrec
    def buildTower(state: State, directions: List[Direction], limit: Long): State =
        state.rockCount match {
            case c if c == limit => state
            case _ =>
                val shape = nextShape(state.rockCount)
                val initShape = translate(2, -4)(shape, state.chamber).toOption.get
                val st = fallingRock(state, initShape, directions)
                buildTower(st, directions, limit)
        }

    @tailrec
    def cycleLength(state: State, directions: List[Direction], increments: List[Int]): State = {
        def isCycle: Boolean = {
            if (increments.length < 10) false
            else {
                val split = increments.splitAt(increments.length / 2)
                !split._1.zip(split._2).exists(x => x._1 != x._2)
            }
        }

        state.rockCount match {
            case _ if isCycle => state
            case c =>
                val shape = nextShape(state.rockCount)
                val initShape = translate(2, -4)(shape, state.chamber).toOption.get
                val st = fallingRock(state, initShape, directions)
                val increases = if (c >= directions.length) (st.chamber.head.length - state.chamber.head.length) +: increments else increments
                cycleLength(st, directions, increases)
        }
    }

    def nextShape(count: Int): Shape =
        count % 5 match {
            case 0 => List(Position(0,0), Position(1, 0), Position(2, 0), Position(3, 0))
            case 1 => List(Position(1,0), Position(0, -1), Position(1, -1), Position(2, -1), Position(1, -2))
            case 2 => List(Position(0,0), Position(1, 0), Position(2, 0), Position(2, -1), Position(2, -2))
            case 3 => List(Position(0,0), Position(0, -1), Position(0, -2), Position(0, -3))
            case 4 => List(Position(0,0), Position(0, -1), Position(1, 0), Position(1, -1))
        }

    def translate(x: Int, y: Int)(shape: Shape, chamber: Chamber): Either[Shape, Shape] =
        shape.map(p => Position(p.x + x, p.y + y)) match {
            case s if !isValidPosition(s, chamber) => Left(shape)
            case s => Right(s)
        }

    @tailrec
    def fallingRock(state: State, shape: Shape, directions: List[Direction]): State = {
        val s = nextDirection(directions, state.movementCount).move(shape, state.chamber) match {
            case Right(s) => s
            case Left(s) => s
        }
        s match {
            case s => Down.move(s, state.chamber) match {
                case Left(s) =>
                    State(addShapeToTower(s, state.chamber), state.rockCount + 1, state.movementCount + 1)
                case Right(s) =>
                    val movementCount = state.movementCount + 1
                    fallingRock(state.copy(movementCount = movementCount), s, directions)
            }
        }
    }

    def isValidPosition(shape: Shape, chamber: Chamber): Boolean =
        !shape.exists(pos => pos.x < 0 || pos.x >= chamber.length || (pos.y >= 0 && chamber(pos.x)(pos.y) == Rock))

    def nextDirection(directions: List[Direction], i: Int): Direction =
        directions(i % directions.length)

    def addShapeToTower(shape: Shape, chamber: Chamber): Chamber = {
        val shapeRows = shape.sortBy(_.y).groupBy(_.y).toList.sortBy(-_._1)
        shapeRows.foldLeft(chamber)((ch, sr) => ch.zipWithIndex.map{
            case (column, x) if sr._1 >= 0 && sr._2.exists(p => p.x == x) => column.updated(sr._1, Rock)
            case (column, _) if sr._1 >= 0 => column
            case (column, x) if sr._2.exists(p => p.x == x) => Rock +: column
            case (column, x) => Air +: column
        })

    }

    @tailrec
    def printChamber(chamber: Chamber, i: Int = 0): Unit = {
        chamber.map(_(i)) match {
            case Nil => ()
            case c =>
                c.foreach {
                    case Air => print('.')
                    case _ => print('#')
                }
                println()
        }
        if (i < chamber.head.length - 1) {
            printChamber(chamber, i + 1)
        } else {
            println("____")
        }
    }

}
