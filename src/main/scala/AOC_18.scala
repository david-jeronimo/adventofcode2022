import scala.annotation.tailrec
import scala.io.Source

object AOC_18 {

    def main(args: Array[String]): Unit = {
        Seq(
            surfaceArea(Source.fromResource("day18_sample.txt")),
            surfaceArea(Source.fromResource("day18_input.txt")),
            surfaceAreaPart2(Source.fromResource("day18_sample.txt")),
            surfaceAreaPart2(Source.fromResource("day18_input.txt")),
        ).foreach(println)
    }

    trait Position { def x: Int; def y: Int; def z: Int}
    case class Cube(x: Int, y: Int, z: Int) extends Position
    case class Air(x: Int, y: Int, z: Int) extends Position

    val movements = Seq[Position => Position](
        c => Cube(c.x + 1, c.y, c.z),
        c => Cube(c.x - 1, c.y, c.z),
        c => Cube(c.x, c.y + 1, c.z),
        c => Cube(c.x, c.y - 1, c.z),
        c => Cube(c.x, c.y, c.z + 1),
        c => Cube(c.x, c.y, c.z - 1)
    )

    def lineToCube(line: String): Cube =
        line.split(",") match {
            case Array(x, y, z) => Cube(x.toInt, y.toInt, z.toInt)
        }

    def surfaceArea(source: Source): Int = {
        val cubes:List[Cube] = source.getLines.toList.map(lineToCube)
        val coveredSides = for {
            cube1 <- cubes
        } yield sidesCovered(cube1, cubes)
        coveredSides.map(6 - _).sum
    }

    def surfaceAreaPart2(source: Source): Int = {
        val cubes:List[Cube] = source.getLines.toList.map(lineToCube)
        val limits = (Cube(cubes.map(_.x).min - 1, cubes.map(_.y).min - 1, cubes.map(_.z).min - 1),
          Cube(cubes.map(_.x).max + 1, cubes.map(_.y).max + 1, cubes.map(_.z).max + 1))
        cubes.map(c => outwardSides(c, cubes, limits)).sum
    }

    def sidesCovered(cube: Cube, cubes: List[Cube]): Int =
        movements.map(_(cube)).intersect(cubes).length

    def outwardSides(cube: Cube, cubes: List[Cube], limits: (Cube, Cube)): Int =
        movements.map(_(cube)).map {
            case c if cubes.contains(c) => 0
            case c if isOutward(List(Air(c.x, c.y, c.z)), cubes, limits, List()) =>
                1
            case _ => 0
        }.sum


    @tailrec
    def isOutward(airList: List[Position], cubes: List[Cube], limits: (Cube, Cube), visited: List[Position]): Boolean =
        airList.distinct match {
            case Nil  => false
            case air::airs if isOutside(air, limits) => true
            case air::airs => isOutward(
                movements.map(_(air))
                .filter(c => !cubes.contains(c))
                  .filter(c => !visited.contains(c))
              .toList ++ airs, cubes, limits, air +: visited)
        }

    def isOutside(air: Position, limits: (Cube, Cube)): Boolean =
        (air.x < limits._1.x || air.x > limits._2.x) ||
          (air.y < limits._1.y || air.y > limits._2.y) ||
          (air.z < limits._1.z || air.z > limits._2.z)

}
