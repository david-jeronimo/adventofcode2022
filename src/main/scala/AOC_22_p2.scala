import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object AOC_22_p2 {

    def main(args: Array[String]): Unit =
        Seq(
            findPasswordPartTwo(Source.fromResource("day22_sample.txt"), 4),
            findPasswordPartTwo(Source.fromResource("day22_input.txt"), 50),
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
    case class Area(id: Int, position: Position, connections: mutable.Map[Compass, (Area, Compass)])

    type AreaMapExistence = Array[Array[Boolean]]
    type AreaMap = List[Area]

    val compassList: Array[Compass] = Array(East, South, West, North)
    sealed trait Compass {
        def turn: Direction => Compass = {
            case Left => compassList((this.value + 3) % 4)
            case Right => compassList((this.value + 1) % 4)
        }
        def next: Position => Position
        def value: Int
        def opposite: Compass = compassList((this.value + 2) % 4)
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

    def findPasswordPartTwo(source: Source, areaWidth: Int): Int = {
        val (map, movements) = populateInput(source.getLines().toList)
        val areaMap = populateAreas(map, areaWidth)

        val emptyCellPositions = for {
            (row, i) <- map.zipWithIndex
            (cell, j) <- row.zipWithIndex
            if cell == Empty
        } yield Position(j, i)
        val startPosition = emptyCellPositions.minBy(p => (p.y, p.x))
        val result = movements.foldLeft(State(startPosition, East))((s, m) => findPath(map, areaMap, areaWidth, s, m))
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

    def populateMovements(line: String): List[Movement] =
        line.foldLeft(List[Movement]())(
            (movements, c) => movements match {
                case Nil => List(Forward(c - '0'))
                case Turn(_)::_ => Forward(c - '0') +: movements
                case Forward(_)::_ if c == 'L' => Turn(Left) +: movements
                case Forward(_)::_ if c == 'R' => Turn(Right) +: movements
                case Forward(n)::ms => Forward(n * 10 + (c - '0')) +: ms
            }
        ).reverse

    def findPath(world: World, areaMap: AreaMap, width: Int, state: State, movement: Movement): State =
        (state.position, state.compass, movement) match {
            case (p, c, Turn(d)) => State(p, c.turn(d))
            case (p, c, Forward(n)) =>
                val (pos, comp) = move(world, areaMap, width, p, c, n)
                State(pos, comp)
        }

    @tailrec
    def move(world: World, areaMap: AreaMap, width: Int, position: Position, compass: Compass, steps: Int): (Position, Compass) =
                steps match {
                    case 0 => (position, compass)
                    case _ =>
                        val (nextPos, nextCompass) = nextPosition(world, areaMap, width, position, compass)
                        world(nextPos.y)(nextPos.x) match {
                              case Wall => (position, compass)
                              case Empty => move(world, areaMap, width, nextPos, nextCompass, steps - 1)
                          }
                }

    def nextPosition(world: World, areaMap: AreaMap, width: Int, position: Position, compass: Compass): (Position, Compass) = {
        val nextPosition = compass.next(position)
        world.lift(nextPosition.y).flatMap(_.lift(nextPosition.x)).getOrElse(Nothing) match {
            case Nothing => teleport(areaMap, width, position, compass)
            case _ => (nextPosition, compass)
        }
    }

    def teleport(areaMap: AreaMap, width: Int, position: Position, compass: Compass): (Position, Compass) = {
        val area: Area = areaMap.find(_.position == Position(position.x / width, position.y / width)).get
        val (newArea, newCompass) = area.connections(compass)
        val newAreaPosition = areaMap.find(_ == newArea).get.position
        val reverse: Boolean = (compass, newCompass) match {
            case (East, South) | (West, North) | (North, West) | (South, East) | (South, North) | (East, West) | (West, East) => true
            case _ => false
        }
        val offset = (compass, reverse) match {
            case (North | South, true) => width - (position.x % width) - 1
            case (North | South, false) => position.x % width
            case (East | West, true) => width - (position.y % width) - 1
            case (East | West, false) => position.y % width

        }
        newCompass match {
            case South => (Position(newAreaPosition.x * width + offset,   newAreaPosition.y * width), newCompass)
            case North => (Position(newAreaPosition.x * width + offset,   newAreaPosition.y * width + width - 1), newCompass)
            case East => (Position(newAreaPosition.x * width,             newAreaPosition.y * width + offset), newCompass)
            case West => (Position(newAreaPosition.x * width + width - 1, newAreaPosition.y * width + offset), newCompass)
        }
    }

    def populateAreas(world: World, areaWidth: Int): List[Area] = {
        def populateAreaMapExistence(world: World, areaWidth: Int): AreaMapExistence =
            (0 until world.length / areaWidth).map(
                j => (0 until world.maxBy(_.length).length / areaWidth).map (
                    i => world(j * areaWidth).length > (i * areaWidth) &&
                      world(j * areaWidth)(i * areaWidth) != Nothing
                ).toArray
            ).toArray

        def linkAreas(area1: Area, area2: Area, compassFromArea1: Compass, compassToArea2: Compass, compassFromArea2: Compass): Unit = {
            area1.connections.put(compassFromArea1, (area2, compassToArea2))
            area2.connections.put(compassFromArea2, (area1, compassFromArea1.opposite))
        }

        def populateDirectConnections(areas: List[Area]): Unit =
            areas.foreach(area => {
                compassList.foreach(
                    compass => areas.find(a => a.position == compass.next(area.position)).map(
                        area2 => linkAreas(area, area2, compass, compass, compass.opposite)
                    )
                )
            })

        def populateIndirectConnections(areas: List[Area]): Unit =
            areas.foreach(
                area => compassList.foreach {
                    compass => if (!area.connections.contains(compass)) {
                        area.connections.get(compass.turn(Left)).map(_._1).foreach {
                            connectingArea =>
                                connectingArea.connections.get(compass).foreach {
                                    case (area2, connectingCompass) if area != area2 && !area.connections.exists(_._2._1 == area2) =>
                                        linkAreas(area, area2, compass, connectingCompass.turn(Left), connectingCompass.turn(Right))
                                    case _ => ()
                                }
                        }
                        area.connections.get(compass.turn(Right)).map(_._1).foreach {
                            connectingArea =>
                                connectingArea.connections.get(compass).foreach {
                                    case (area2, connectingCompass) if area != area2 && !area.connections.exists(_._2._1 == area2) =>
                                        linkAreas(area, area2, compass, connectingCompass.turn(Right), connectingCompass.turn(Left))
                                    case _ => ()
                                }
                        }
                    }
                }
            )

        val areaMapExistence = populateAreaMapExistence(world, areaWidth)
        val areaPositions = for {
            (row, j) <- areaMapExistence.zipWithIndex
            (cell, i) <- row.zipWithIndex
            if cell
        } yield Position(i, j)

        val areas = areaPositions.zipWithIndex.map(e => Area(e._2, e._1, mutable.Map[Compass, (Area, Compass)]())).toList
        populateDirectConnections(areas)
        (0 to 3).foreach(_ => populateIndirectConnections(areas))
        areas
    }

}