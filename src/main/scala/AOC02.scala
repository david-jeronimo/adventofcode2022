import scala.io.Source

sealed trait Shape {
  def score: Int
  def defeats: Shape
  def defeatedBy: Shape
}
case object Rock extends Shape { val score = 1; val defeats = Scissors; val defeatedBy = Paper }
case object Paper extends Shape { val score = 2; val defeats = Rock; val defeatedBy = Scissors }
case object Scissors extends Shape { val score = 3; val defeats = Paper; val defeatedBy = Rock }

sealed trait Result {
  def score: Int
}
case object Win extends Result { val score = 6 }
case object Draw extends Result { val score = 3 }
case object Lose extends Result { val score = 0 }

case class Round (shape: Shape, result: Result)

object Aoc02 {

  def main(args: Array[String]): Unit = {
    System.out.println(
      calculateScore(Source.fromResource("day2_input.txt"))
    )
  }

  implicit def charToShape(char: Char): Shape = char match {
    case 'A' => Rock
    case 'B' => Paper
    case _ => Scissors
  }

  implicit def charToResult(char: Char): Result = char match {
    case 'X' => Lose
    case 'Y' => Draw
    case _ => Win
  }

  private def calculateScore(source: Source): Int = {
    implicit def roundFromLine(line: String): Round = Round(line.charAt(0), line.charAt(2))

    def calculateShape(round: Round): Shape = round match {
      case Round(s, Draw) => s
      case Round(s, Lose) => s.defeats
      case Round(s, _) => s.defeatedBy
    }

    def roundPoints(round: Round): Int = calculateShape(round).score + round.result.score

    source.getLines().foldLeft(0)(
      (accPoints, line) => accPoints + roundPoints(line)
    )
  }
}