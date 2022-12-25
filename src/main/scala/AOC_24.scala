import scala.annotation.tailrec
import scala.io.Source
import scala.math.abs

object AOC_24 {

    def main(args: Array[String]): Unit =
        Seq(
            roundsTofindExit(Source.fromResource("day24_sample.txt")),
            roundsTofindExit(Source.fromResource("day24_input.txt")),
            roundsTofindExitPartTwo(Source.fromResource("day24_sample.txt")),
            roundsTofindExitPartTwo(Source.fromResource("day24_input.txt")),
        ).foreach(println)

    sealed trait Direction { def move: Position => Position}
    case object Up extends Direction { val move = p => Position(p.x, p.y - 1)}
    case object Down extends Direction { val move = p => Position(p.x, p.y + 1)}
    case object Left extends Direction { val move = p => Position(p.x - 1, p.y)}
    case object Right extends Direction { val move = p => Position(p.x + 1, p.y)}
    val directions = List(Up, Down, Left, Right)

    case class Position(x: Int, y: Int)
    case class Blizzard(position: Position, direction: Direction){
        def move(implicit max: (Int, Int)): Blizzard = {
            val position: Position = direction.move(this.position) match {
                case Position(-1, y) => Position(max._1, y)
                case Position(x, -1) => Position(x, max._2)
                case Position(x, y) if x > max._1 => Position(0, y)
                case Position(x, y) if  y > max._2 => Position(x, 0)
                case p => p
            }
            Blizzard(position, this.direction)
        }
    }

    def roundsTofindExit(source: Source): Int = {
        val lines = source.getLines().toList
        val blizzards = populateBlizzards(lines)
        implicit val limits: (Int, Int) = (lines.head.length - 3, lines.length - 3)
        findPath(List(Position(0, -1)), blizzards, Position(limits._1, limits._2 + 1)) - 1
    }

    def roundsTofindExitPartTwo(source: Source): Int = {
        val lines = source.getLines().toList
        val blizzards = populateBlizzards(lines)
        implicit val limits: (Int, Int) = (lines.head.length - 3, lines.length - 3)
        findPath(List(Position(0, -1)), blizzards, Position(limits._1, limits._2 + 1), tripsLeft = 2)
    }

    def populateBlizzards(lines: List[String]): List[Blizzard] = {
        implicit def charToDirection(c: Char): Option[Direction] = c match {
            case '^' => Some(Up)
            case 'v' => Some(Down)
            case '<' => Some(Left)
            case '>' => Some(Right)
            case _ => None
        }

        lines.drop(1).dropRight(1).zipWithIndex.flatMap {
            case (line, j) => line
              .drop(1).dropRight(1)
              .map(c => charToDirection(c))
              .zipWithIndex
              .flatMap(e => e._1.map(d => Blizzard(Position(e._2, j), d)))
        }
    }

    @tailrec
    def findPath(candidates: List[Position], blizzards: List[Blizzard], exit: Position, count: Int = 0, tripsLeft: Int = 0)
                (implicit max: (Int, Int)): Int =
        (tripsLeft, candidates.sortBy(p => (abs(p.x - exit.x), abs(p.y - exit.y)))) match {
            case (0, c::_) if c == exit => count
            case (2, c::_) if c == exit => findPath(findCandidates(c, blizzards), blizzards.map(_.move), Position(0, -1), count + 1, 1)
            case (1, c::_) if c == exit => findPath(findCandidates(c, blizzards), blizzards.map(_.move), Position(max._1, max._2), count + 1, 0)
            case (_, candidates) => findPath(candidates.flatMap(findCandidates(_, blizzards)).distinct, blizzards.map(_.move), exit, count + 1, tripsLeft)
        }

    def findCandidates(position: Position, blizzards: List[Blizzard])(implicit max: (Int, Int)): List[Position] =
        (directions.map(_.move(position)) :+ position)
          .filter(isValidPosition(_, blizzards))
          .distinct

    def isValidPosition(position: Position, blizzards: List[Blizzard])(implicit max: (Int, Int)): Boolean =
        position.x == 0 && position.y == -1 ||
          (position.x == max._1 && position.y == max._2 + 1) ||
          (position.x >= 0 && position.y >= 0 && position.x <= max._1 && position.y <= max._2 &&
          !blizzards.map(_.position).contains(position))

}
