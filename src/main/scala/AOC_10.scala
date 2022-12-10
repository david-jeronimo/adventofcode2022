import scala.io.Source
import scala.math.abs

object AOC_10 {

    def main(args: Array[String]): Unit = {
        // part one
        Seq(
            signalStrength(Source.fromResource("day10_sample.txt")),
            signalStrength(Source.fromResource("day10_input.txt"))
        ).foreach(println)

        // part two
        drawCrt(Source.fromResource("day10_sample.txt"))
        drawCrt(Source.fromResource("day10_input.txt"))
    }

    trait Op
    case object NoOp extends Op
    case class Add(x: Int) extends Op

    case class State(x: Int, previousResult: Option[Int], results: Seq[Int], cycles: Seq[Int])

    def signalStrength(source: Source): Int =
        source.getLines()
          .map(lineToOp)
          .foldLeft(State(1, None, Seq(), Seq(0)))(tick)
          .results.sum

    def drawCrt(source: Source): Unit =
        source.getLines()
          .map(lineToOp)
          .scanLeft(State(1, None, Seq(), Seq(0)))(tick)
          .drop(1)
          .flatMap(s => s.cycles.map(
              c => ((c - 1) % 40 , s.x)
          ))
          .map {
              case (sprite, x) if abs(sprite - x) <= 1 => '#'
              case _ => '.'
          }
          .grouped(40)
          .foreach(group => {
              group.foreach(print)
              println()
          })

    def lineToOp(line: String): Op =
        line.split(" ") match {
            case Array("noop") =>NoOp
            case Array("addx", n) => Add(n.toInt)
        }

    def tick(state: State, op: Op): State = {
        val x = state.previousResult.getOrElse(state.x)
        val (opResult, nextCycles) = (x, op, state.cycles.last) match {
            case (_, NoOp, cycle) => (None, Seq(cycle + 1))
            case (x, Add(n), cycle) => (Some(x + n), Seq(cycle + 1, cycle + 2))
        }
        val results = nextCycles.intersect(Seq(20, 60, 100, 140, 180, 220)).headOption match {
            case Some(c) => c * x +: state.results
            case None => state.results
        }
        State(x, opResult, results, nextCycles)
    }

}
