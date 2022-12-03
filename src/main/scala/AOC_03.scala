import scala.io.Source

object AOC_03 {

    def main(args: Array[String]): Unit = {

        System.out.println(
            findSumOfPriorities(Source.fromResource("day3_input.txt"))
        )
    }

    def findSumOfPriorities(source: Source): Int =
        source.getLines().foldLeft(0)(linePriority)

    def linePriority(acc: Int, line: String): Int  = {
        val compartments: (Seq[Char], Seq[Char]) = line.toSeq.splitAt(line.length / 2)
        val duplicate: Char = compartments._1.intersect(compartments._2).head
        acc + charPriority(duplicate)
    }

    def charPriority(char: Char): Int = {
        char match {
            case c if c.isLower => c - 'a' + 1
            case c => c - 'A' + 27
        }
    }

}
