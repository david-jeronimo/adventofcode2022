import scala.annotation.tailrec
import scala.io.Source
import scala.math._

object AOC_14 {

    def main(args: Array[String]): Unit = {
        Seq(
            unitsOfSand(Source.fromResource("day14_sample.txt"), PartOne),
            unitsOfSand(Source.fromResource("day14_input.txt"), PartOne),
            unitsOfSand(Source.fromResource("day14_sample.txt"), PartTwo),
            unitsOfSand(Source.fromResource("day14_input.txt"), PartTwo),
        ).foreach(println)
    }

    trait Cell
    case object Rock extends Cell
    case object Air extends Cell
    case object Sand extends Cell

    type Row = Array[Cell]
    type Cave = Array[Row]
    case class Position(x: Int, y:Int)
    type RockPath = List[Position]

    trait SandOverflow
    case object overflow extends SandOverflow
    case object filledIn extends SandOverflow

    trait Part
    case object PartOne extends Part
    case object PartTwo extends Part

    def unitsOfSand(source: Source, part: Part): Int = {
        val rockPaths = source.getLines().map(lineToRockPath).toSeq
        val minX = rockPaths.flatMap(rp => rp.map(_.x)).min
        val maxX = rockPaths.flatMap(rp => rp.map(_.x)).max
        val maxY = rockPaths.flatMap(rp => rp.map(_.y)).max
        val margin = part match {
            case PartOne => 2
            case PartTwo => maxY
        }

        val emptyCave = Array.tabulate[Cell](maxY + 1, maxX - minX + margin * 2)((_,_) => Air)
        val cave = rockPaths.map(rp => rp.map(r => Position(r.x - minX + margin, r.y)))
          .foldLeft(emptyCave)(populateCave)

        val c = expandCave(cave, margin, part)
        throwSand(c, 500 - minX + margin * 2, 0)

    }

    def lineToRockPath (line: String): RockPath =
        line.split(" -> ").map(
            coord => coord.split(",") match {
                case Array(x, y) => Position(x.toInt, y.toInt)
            }
        ).toList

    @tailrec
    def populateCave (cave: Cave, rockPath: RockPath): Cave = {
        def addPathToRow(row: Row, fromX: Int, toX: Int): Row =
            (min(fromX, toX) to max(fromX, toX)).foldLeft(row)(
                (r, i) => r.updated(i, Rock))

        def addPathToColumn(cave: Cave, x: Int, fromY: Int, toY: Int): Cave =
            (min(fromY, toY) to max(fromY,toY)).foldLeft(cave)(
                (c, j) => c.updated(j, c(j).updated(x, Rock)))

        rockPath match {
            case _::Nil => cave
            case Position(x1, y1)::Position(x2, y2)::_ if y1 == y2 =>
                val c = cave.updated(y1, addPathToRow(cave(y1), x1, x2))
                populateCave(c, rockPath.drop(1))
            case Position(x1, y1)::Position(_, y2)::_ =>
                val c = addPathToColumn(cave, x1, y1, y2)
                populateCave(c, rockPath.drop(1))
        }
    }

    @tailrec
    def throwSand(cave: Cave, sandX: Int, count: Int): Int =
        sandFalls(cave, Position(sandX, 0)) match {
            case Left(o) if o == overflow => count
            case Left(o) if o == filledIn => count + 1
            case Right(c) => throwSand(c, sandX, count + 1)
        }

    def sandFalls(cave: Cave, position: Position): Either[SandOverflow, Cave] =
        nextSandPosition(cave, position) match {
            case Left(so) => Left(so)
            case Right(Position(_, 0)) => Left(filledIn)
            case Right(p) => Right(cave.updated(p.y, cave(p.y).updated(p.x, Sand)))
        }

    @tailrec
    def nextSandPosition(cave: Cave, position: Position): Either[SandOverflow, Position] =
        Seq[Position => Position](
            p => Position(p.x, p.y + 1),
            p => Position(p.x - 1, p.y + 1),
            p => Position(p.x + 1, p.y + 1),
        ).map(_ (position))
          .map {
              case Position(x, y) if y >= cave.length => Left(overflow)
              case Position(x, y) if cave(y)(x) == Air => Right(Some(Position(x, y)))
              case _ => Right(None)
          }.find(r => r != Right(None)) match {
            case Some(Left(_)) => Left(overflow)
            case Some(Right(Some(p))) => nextSandPosition(cave, p)
            case None => Right(position)
        }

    def expandCave(cave: Cave, margin: Int, part: Part): Cave = {
        val c = cave.map(row => Array.fill(margin)(Air) ++ row ++ Array.fill(margin)(Air))
        part match {
            case PartOne => c
            case PartTwo => c ++ Array[Row](Array.fill(cave(0).length + margin * 2)(Air),
                Array.fill(cave(0).length + margin * 2)(Rock))
        }
    }

    def printCave(cave: Cave): Unit =
        cave.foreach(
            row => {
                row.foreach {
                    case Air => print(".")
                    case Rock => print("#")
                    case _ => print("o")
                }
                println()
            })
}
