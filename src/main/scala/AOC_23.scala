import scala.annotation.tailrec
import scala.io.Source

object AOC_23 {

    def main(args: Array[String]): Unit =
        Seq(
            emptyTiles(Source.fromResource("day23_sample.txt")),
            emptyTiles(Source.fromResource("day23_input.txt")),
            emptyTilesPartTwo(Source.fromResource("day23_sample.txt")),
            emptyTilesPartTwo(Source.fromResource("day23_input.txt")),
        ).foreach(println)

    case class Position(x: Int, y: Int)

    sealed trait Direction {
        def requiredPositions: Position => List[Position]
        def next: Position => Position
    }
    case object North extends Direction {
        override def requiredPositions: Position => List[Position] = p => List(Position(p.x - 1, p.y - 1), Position(p.x, p.y - 1), Position(p.x + 1, p.y - 1))
        override def next: Position => Position = p => Position(p.x, p.y - 1)
    }
    case object East extends Direction {
        override def requiredPositions: Position => List[Position] = p => List(Position(p.x + 1, p.y - 1), Position(p.x + 1, p.y), Position(p.x + 1, p.y + 1))
        override def next: Position => Position = p => Position(p.x + 1, p.y)
    }
    case object West extends Direction {
        override def requiredPositions: Position => List[Position] = p => List(Position(p.x - 1, p.y - 1), Position(p.x - 1, p.y), Position(p.x - 1, p.y + 1))
        override def next: Position => Position = p => Position(p.x - 1, p.y)
    }
    case object South extends Direction {
        override def requiredPositions: Position => List[Position] = p => List(Position(p.x - 1, p.y + 1), Position(p.x, p.y + 1), Position(p.x + 1, p.y + 1))
        override def next: Position => Position = p => Position(p.x, p.y + 1)
    }

    val directions = Array(North, South, West, East)

    def emptyTiles(source: Source): Int = {
        val elfPositions = linesToPositions(source.getLines().toList)
        val r = spread(elfPositions, 0, 10)
        (r.maxBy(_.x).x - r.minBy(_.x).x + 1) * (r.maxBy(_.y).y - r.minBy(_.y).y + 1) - r.length
    }

    def emptyTilesPartTwo(source: Source): Int = {
        val elfPositions = linesToPositions(source.getLines().toList)
        spreadPartTwo(elfPositions, 0) + 1
    }

    def linesToPositions(lines: List[String]): List[Position] =
        lines.zipWithIndex.flatMap {
            case (line, j) => line.zipWithIndex.flatMap{
                case ('#', i) => Some(Position(i, j))
                case _ => None
            }
        }

    @tailrec
    def spread(elfs: List[Position], count: Int, limit: Int): List[Position] =
        count match {
            case n if n == limit => elfs
            case _ =>
                val oldNewPositions = for {
                    elf <- elfs
                    elfNext = nextPosition(elfs, count % 4)(elf)
                } yield (elf, elfNext)
                val positions = oldNewPositions.map(ep => (ep, oldNewPositions.count(_._2 == ep._2))) map {
                    case ((_, newPos), 1) => newPos
                    case ((old, _), _) => old
                }
                spread(positions, count + 1, limit)
        }

    @tailrec
    def spreadPartTwo(elfs: List[Position], count: Int): Int = {
        val oldNewPositions = for {
            elf <- elfs
            elfNext = nextPosition(elfs, count % 4)(elf)
        } yield (elf, elfNext)
        val positions = oldNewPositions.map(ep => (ep, oldNewPositions.count(_._2 == ep._2))) map {
            case ((_, newPos), 1) => newPos
            case ((old, _), _) => old
        }
        if (positions.intersect(elfs).length == elfs.length)
            count
        else
            spreadPartTwo(positions, count + 1)

    }

    @tailrec
    def nextPosition(elfs: List[Position], preferredDirection: Int, count: Int = 4)(elf: Position): Position =
        (count, adjacentElfs(elf, elfs)) match {
            case (0, _) => elf
            case (_, false) => elf
            case _ =>
                val direction = directions(preferredDirection)
                (count, direction.requiredPositions(elf).find(rp => elfs.contains(rp))) match {
                    case (0, _) => elf
                    case (_, None) => direction.next(elf)
                    case _ => nextPosition(elfs, (preferredDirection + 1) % 4, count - 1)(elf)
                }
        }

    def adjacentElfs(elf: Position, elfs: List[Position]): Boolean =
        (North.requiredPositions(elf) ++ South.requiredPositions(elf) :+ East.next(elf) :+ West.next(elf))
          .exists(p => elfs.contains(p))

    def printElfs(elfs: List[Position]): Unit =
        (-5 to 15).foreach(j => {
            (-5 to 15).foreach(i =>
                elfs.find(_ == Position(i, j)) match {
                    case Some(_) => print("#")
                    case _ => print(".")
                }
            )
            println()
        })

}
