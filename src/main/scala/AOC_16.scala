import scala.annotation.tailrec
import scala.io.Source

object AOC_16 {

    def main(args: Array[String]): Unit =
        Seq(
            releasedPressure(Source.fromResource("day16_sample.txt")),
            releasedPressure(Source.fromResource("day16_input.txt")),

        ).foreach(println)

    case class Valve(name: String, rate: Int)
    case class State(valve: Valve, accumulatedFlow: Int, openValves: List[Valve], log: List[String])
    type Tunnels = Map[String, List[Valve]]
    type Action = State => State

    def releasedPressure(source: Source): Int = {
        val (tunnels, startValve) = populateValves(source)
        release(tunnels, List(State(startValve, 0, List(), List())), 30)
    }

    def populateValves(source: Source): (Tunnels, Valve) = {
        val input: List[(Valve, List[String])] = for {
            line <- source.getLines().toList
            (valve, connectedValves) = lineToValve(line)
        } yield (valve -> connectedValves)
        val tunnels = input.map(entry => entry._1.name -> entry._2.map(
            cv => input.find(_._1.name == cv).get._1)
        ).sortBy(_._1).toMap
        (tunnels, input.find(p => p._1.name == "AA").map(_._1).get)
    }

    def lineToValve(line: String): (Valve, List[String]) = {
        val twoParts: Seq[String] = line.split(";")
        val valve = twoParts.headOption.map(s => {
            val words: Seq[String] = s.split (" ")
            Valve(words (1), words.last.split ("=").last.toInt)
        }).get
        val connectedValves: List[String] = twoParts.last.split(" to ").last.split(" ").drop(1).map(_.take(2)).toList
        (valve, connectedValves)
    }

    @tailrec
    def release(tunnels: Tunnels, states: List[State], count: Int): Int =
        (states.sortBy(-_.accumulatedFlow).take(10_000), count) match {
            case (st, 0) =>
                st.map(_.accumulatedFlow).max
            case (sts, c) => {
                val nextStates = sts.flatMap(s => nextActions(tunnels, s).map(f => f(s)))
                release(tunnels, nextStates, c - 1)
            }
        }

    def nextActions(tunnels: Tunnels, state: State): List[Action] =
        (state.valve, state.openValves, tunnels(state.valve.name)) match {
            case (valve, openValves, connectedValves) if openValves.contains(valve) || valve.rate == 0 =>
                connectedValves.sortBy(-_.rate).map(cv => move(cv))
            case (valve, _, connectedValves) =>
                openValve(valve)_ +: connectedValves.sortBy(-_.rate).map(cv => move(cv)_)
        }

    def move(toValve: Valve)(state: State): State =
        state.copy(valve = toValve, accumulatedFlow = increaseFlow(state), log = s"move to ${toValve.name}, flow=${increaseFlow(state)}" +: state.log)

    def openValve(valve: Valve)(state: State): State = 
        state.copy(openValves = valve +: state.openValves, accumulatedFlow = increaseFlow(state), log = s"open ${valve.name}, flow=${increaseFlow(state)}" +: state.log)

    def increaseFlow(state: State): Int =
        state.accumulatedFlow + state.openValves.map(_.rate).sum
}
