import scala.annotation.tailrec
import scala.io.Source
import scala.math.abs

object AOC_09 {

    def main(args: Array[String]): Unit = {
        Seq(
            countTailPositions(Source.fromResource("day9_input.txt"), 2), //part one
            countTailPositions(Source.fromResource("day9_input.txt"), 10) // part two
        ).foreach(println)
    }

    case class Position(x: Int, y: Int)
    case class Model(knots: Seq[Position], tail: Seq[Position])

    trait Direction
    case object Left extends Direction
    case object Right extends Direction
    case object Up extends Direction
    case object Down extends Direction
    case class Movement(direction: Direction, times: Int)

    implicit def charToDirection(char: String): Direction = char(0) match {
        case 'L' => Left
        case 'R' => Right
        case 'U' => Up
        case _ => Down
    }

    def countTailPositions(source: Source, numKnots: Int): Int =
        source.getLines()
          .map(lineToMovement)
          .foldLeft(Model(
              knots = Seq.fill(numKnots - 1)(Position(0, 0)),
              tail = Seq(Position(0, 0))
          )) (move)
          .tail.distinct.length

    def lineToMovement(line: String): Movement =
        line.split(" ") match {
            case Array(d, t) => Movement(d, t.toInt)
        }

    @tailrec
    def move(model: Model, movement: Movement): Model =
        movement match {
            case Movement(_, 0) => model
            case Movement(d, n) =>
                val headPosition = moveHead(model.knots.head, d)
                val knotsPositions = (model.knots.drop(1) :+ model.tail.head).scanLeft(headPosition)(moveKnot)
                move(
                    Model(knotsPositions.dropRight(1), tail = knotsPositions.last +: model.tail),
                    Movement(d, n - 1)
                )
        }

    def moveHead(position: Position, direction: Direction): Position =
        (position, direction) match {
            case (Position(x, y), Up) => Position(x, y + 1)
            case (Position(x, y), Down) => Position(x, y - 1)
            case (Position(x, y), Left) => Position(x - 1, y)
            case (Position(x, y), Right) => Position(x + 1, y)
        }

    def moveKnot(prevPosition: Position, knotPosition: Position): Position = {

        def moveDimension(d1: Int, d2: Int, dist: Int = 1): Int =
            d1 - d2 match {
                case distance if distance < -dist => d2 - 1
                case distance if distance > dist => d2 + 1
                case _ => d2
            }

        def moveDiagonal(tailPosition: Position): Position =
            Position(
                moveDimension(prevPosition.x, tailPosition.x, 0),
                moveDimension(prevPosition.y, tailPosition.y, 0)
            )

        def shouldNotMove(tailPosition: Position): Boolean =
            abs(tailPosition.x - prevPosition.x) <=1 && abs(tailPosition.y - prevPosition.y) <=1

        (prevPosition, knotPosition) match {
            case (_, t) if shouldNotMove(t) => t
            case (Position(hx, hy), Position(tx, ty)) if (hx != tx && hy != ty) => moveDiagonal(knotPosition)
            case _ => Position(
                moveDimension(prevPosition.x, knotPosition.x),
                moveDimension(prevPosition.y, knotPosition.y)
            )
        }
    }
}
