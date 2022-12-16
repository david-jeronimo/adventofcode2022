import scala.annotation.tailrec
import scala.io.Source

object AOC_16_p2 {

    def main(args: Array[String]): Unit =
        Seq(
            releasedPressure(Source.fromResource("day16_sample.txt")),
            releasedPressure(Source.fromResource("day16_input.txt"))
        ).foreach(println)

    case class Valve(name: String, rate: Int)
    case class State(valve1: Valve, valve2: Valve, accumulatedFlow: Int, openValves: List[Valve])
    type Tunnels = Map[String, List[Valve]]
    type Action = State => State

    def releasedPressure(source: Source): Int = {
        val (tunnels, startValve) = populateValves(source)
        release(tunnels, List(State(startValve, startValve, 0, List())), 26)
    }

    def populateValves(source: Source): (Tunnels, Valve) = {
        val input: List[(Valve, List[String])] = for {
            line <- source.getLines().toList
            (valve, connectedValves) = lineToValve(line)
        } yield valve -> connectedValves
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
            case (st, 0) => st.map(_.accumulatedFlow).max
            case (sts, c) =>
                val nextStates = for {
                    state <- sts
                    accumulatedFlow = increaseFlow(state)
                    (action1, action2) <- nextActions(tunnels, state)
                    newState = action2(action1(state))
                } yield newState.copy(accumulatedFlow = accumulatedFlow)
                release(tunnels, nextStates, c - 1)
        }

    def nextActions(tunnels: Tunnels, state: State): List[(Action, Action)] =
        for {
            action1 <- nextActions(tunnels, state.valve1, state.openValves, move1)
            action2 <- nextActions(tunnels, state.valve2, state.openValves, move2)
        } yield (action1, action2)

    def nextActions(tunnels: Tunnels, valve: Valve, openValves: List[Valve], moveAction: Valve => State => State): List[Action] =
        (valve, openValves, tunnels(valve.name).sortBy(-_.rate).map(cv => moveAction(cv))) match {
            case (valve, openValves, moveActions) if openValves.contains(valve) || valve.rate == 0 => moveActions
            case (valve, _, moveActions) => openValve(valve)_ +: moveActions
        }

    def move1(toValve: Valve)(state: State): State =
        state.copy(valve1 = toValve)

    def move2(toValve: Valve)(state: State): State =
        state.copy(valve2 = toValve)

    def openValve(valve: Valve)(state: State): State = {
        if (state.openValves.contains(valve)) state
        else state.copy(openValves = valve +: state.openValves)
    }

    def increaseFlow(state: State): Int =
        state.accumulatedFlow + state.openValves.map(_.rate).sum
}
