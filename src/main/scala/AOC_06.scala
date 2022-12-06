import scala.io.Source

object AOC_06 {

    val MarkerLength = 14 // 4 for part one

    def main(args: Array[String]): Unit =
        Source.fromResource("day6_input.txt").getLines()
          .map(findMarkerIndex)
          .foreach(System.out.println)

    def findMarkerIndex(input: String): Option[Int] =
        input.zipWithIndex.sliding(MarkerLength)
          .find(ci => isMarker(ci.map(_._1)))
          .map(_.last._2 + 1)

    def isMarker(marker: Seq[Char]): Boolean =
        marker.distinct.length == marker.length

}
