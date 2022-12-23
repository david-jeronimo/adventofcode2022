import scala.annotation.tailrec
import scala.io.Source

object AOC_22 {

    def main(args: Array[String]): Unit =
        Seq(
            findPassword(Source.fromResource("day22_sample.txt")),
            findPassword(Source.fromResource("day22_input.txt")),
        ).foreach(println)

    sealed trait Cell
    case object Nothing extends Cell
    case object Empty extends Cell
    case object Wall extends Cell
    type World = Array[Array[Cell]]

    sealed trait Direction
    case object Left extends Direction
    case object Right extends Direction

    sealed trait Movement
    case class Forward(times: Int) extends Movement
    case class Turn(direction: Direction) extends Movement

    case class Position(x: Int, y: Int)

    val compassList: Array[Compass] = Array(East, South, West, North)
    sealed trait Compass {
        def turn: Direction => Compass = {
            case Left => compassList((this.value + 3) % 4)
            case Right => compassList((this.value + 1) % 4)
        }
        def next: Position => Position
        def value: Int
    }
    case object North extends Compass {
        val next = p => Position(p.x, p.y - 1)
        val value = 3
    }
    case object South extends Compass {
        val next = p => Position(p.x, p.y + 1)
        val value = 1
    }
    case object East extends Compass {
        val next = p => Position(p.x + 1, p.y)
        val value = 0
    }
    case object West extends Compass {
        val next = p => Position(p.x - 1, p.y)
        val value = 2
    }
    case class State(position: Position, compass: Compass)

    def findPassword(source: Source): Int = {
        val (map, movements) = populateInput(source.getLines().toList)
        val emptyCellPositions = for {
            (row, i) <- map.zipWithIndex
            (cell, j) <- row.zipWithIndex
            if cell == Empty
        } yield Position(j, i)
        val startPosition = emptyCellPositions.minBy(p => (p.y, p.x))
        val result = movements.foldLeft(State(startPosition, East))((s, m) =>findPath(map, s, m))
        (result.position.y + 1) * 1000 + (result.position.x + 1) * 4 + result.compass.value
    }

    def populateInput(lines: List[String]): (World, List[Movement]) = {
        val (mapStr, movementsStr) = lines.splitAt(lines.indexOf(""))
        (populateMap(mapStr), populateMovements(movementsStr(1)))
    }

    def populateMap(lines: List[String]): World = {
        def charToCell(c: Char): Cell = c match {
            case ' ' => Nothing
            case '.' => Empty
            case '#' => Wall
        }

        lines.map(
            line => line.map(charToCell).toArray
        ).toArray
    }

    def populateMovements(line: String): List[Movement] = {
        line.foldLeft(List[Movement]())(
            (movements, c) => movements match {
                case Nil => List(Forward(c - '0'))
                case Turn(_)::_ => Forward(c - '0') +: movements
                case Forward(_)::_ if c == 'L' => Turn(Left) +: movements
                case Forward(_)::_ if c == 'R' => Turn(Right) +: movements
                case Forward(n)::ms => Forward(n * 10 + (c - '0')) +: ms
            }
        ).reverse
    }

    def findPath(world: World, state: State, movement: Movement): State = {
        (state.position, state.compass, movement) match {
            case (p, c, Turn(d)) => State(p, c.turn(d))
            case (p, c, Forward(n)) => State(move(world, p, c, n), c)
        }
    }

    @tailrec
    def move(world: World, position: Position, compass: Compass, steps: Int): Position = {
        steps match {
            case 0 => position
            case _ =>
                val nextPos: Position = nextPosition(world, position, compass)
                world(nextPos.y)(nextPos.x) match {
                    case Wall => position
                    case Empty => move(world, nextPos, compass, steps - 1)
                }
        }
    }

    def nextPosition(world: World, position: Position, compass: Compass): Position = {
        val nextPosition = compass.next(position)
        world.lift(nextPosition.y).flatMap(_.lift(nextPosition.x)).getOrElse(Nothing) match {
            case Nothing => teleport(world, position, compass.turn(Right).turn(Right))
            case _ => nextPosition
        }
    }

    @tailrec
    def teleport(world: World, position: Position, compass: Compass): Position = {
        val nextPosition = compass.next(position)
        world.lift(nextPosition.y).flatMap(_.lift(nextPosition.x)).getOrElse(Nothing) match {
            case Nothing => position
            case _ => teleport(world, nextPosition, compass)
        }
    }

}
