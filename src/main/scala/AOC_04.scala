import scala.io.Source

object AOC_04 {

    def main(args: Array[String]): Unit = {
        System.out.println(
            numberOfOverlaps(Source.fromResource("day4_input.txt"))
        )
    }

    type Area = (Int, Int)
    type Assignment = (Area, Area)

    def numberOfOverlaps(source: Source): Int =
        source.getLines()
          .map(lineToAssignment)
          .count(overlaps)

    def lineToAssignment(line: String): Assignment = {
        val sections: Seq[Int] = line.split(',').flatMap(a =>
            a.split('-').toSeq.map(b => b.toInt)
        )
        ((sections(0), sections(1)), (sections(2), sections(3)))
    }

    def overlaps(assignment: Assignment): Boolean =
        containsArea(assignment._1, assignment._2) || containsArea(assignment._2, assignment._1)

    def containsArea(area1: Area, area2: Area): Boolean =
        area1._1 <= area2._1 && area1._2 >= area2._2

}


