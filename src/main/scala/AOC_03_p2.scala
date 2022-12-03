import scala.io.Source

object AOC_03_p2 {

    def main(args: Array[String]): Unit = {

        System.out.println(
            findSumOfPriorities(Source.fromResource("day3_input.txt"))
        )
    }

    def findSumOfPriorities(source: Source): Int =
        source.getLines()
          .grouped(3)
          .map(group => group.reduce(_ intersect _).head)
          .map(charPriority)
          .sum

    def charPriority(char: Char): Int = char match {
        case c if c.isLower => c - 'a' + 1
        case c => c - 'A' + 27
    }


}
