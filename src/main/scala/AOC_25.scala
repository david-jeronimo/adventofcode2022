import scala.annotation.tailrec
import scala.io.Source

object AOC_25 {

    def main(args: Array[String]): Unit =
        Seq(
            partOne(Source.fromResource("day25_sample.txt")),
            partOne(Source.fromResource("day25_input.txt")),
        ).foreach(println)

    def partOne(source: Source): String = {
        val decimalResult = source.getLines()
          .map(line => fromSnafu(line.toList))
          .toList.sum
        toSnafu(decimalResult)
    }

    @tailrec
    def fromSnafu(line: List[Char], acc: Long = 0): Long = {
        def charToInt(c: Char): Int = c match {
            case '-' => -1
            case '=' => -2
            case c => c.toInt - '0'
        }

        line match {
            case Nil => acc
            case c::rest => fromSnafu(rest, acc * 5 + charToInt(c))
        }
    }

    @tailrec
    def toSnafu(number: Long, acc: String = ""): String =
        number match {
            case n if n == 0 => acc.reverse
            case n =>
                val c = (number % 5).toInt match {
                    case 4 => '-'
                    case 3 => '='
                    case n => (n + '0').toChar
                }
                toSnafu((n + 2)/ 5, acc :+ c)
        }
}
