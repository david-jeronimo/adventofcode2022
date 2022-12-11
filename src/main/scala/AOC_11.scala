import scala.annotation.tailrec
import scala.io.Source

object AOC_11 {

    def main(args: Array[String]): Unit = {
        Seq(
            monkeyBusiness(Source.fromResource("day11_sample.txt"), PartOne),
            monkeyBusiness(Source.fromResource("day11_input.txt"), PartOne),
            monkeyBusiness(Source.fromResource("day11_sample.txt"), PartTwo),
            monkeyBusiness(Source.fromResource("day11_input.txt"), PartTwo),
        ).foreach(println)
    }

    type Item = Long
    case class Monkey(items: Seq[Item],
                      divisibleBy: Int,
                      worryLevel: Item => Item,
                      throwToMonkey: Item => Int,
                      inspectedItems: Int)

    case class State(monkeys: Seq[Monkey], round: Int)

    trait Part{ def numRounds: Int}
    case object PartOne extends Part {val numRounds = 20}
    case object PartTwo extends Part {val numRounds = 10_000}

    def monkeyBusiness(source: Source, part: Part): BigInt = {
        val monkeys = source.getLines().grouped(7).map(initMonkey(_, part)).toSeq
        val moduleBy = monkeys.map(_.divisibleBy).distinct.product
        val initState = State(
            monkeys.map(
                monkey => monkey.copy(worryLevel = a => monkey.worryLevel(a) % moduleBy)
            ),
            0
        )

        round(initState, part.numRounds)
          .monkeys.map(m => BigInt(m.inspectedItems))
          .sortWith(_ > _).take(2).product
    }

    def initMonkey(lines: Seq[String], part: Part): Monkey = {
        val items = lines(1).split(": ")(1).split(", ").map(_.toLong)
        val operation: Item => Item = lines(2).split(" = old ")(1).split(' ') match {
            case Array("*", "old") => a => a * a
            case Array("*", n) => a => n.toInt * a
            case Array("+", n) => a => n.toInt + a
        }
        val divisibleBy = lines(3).split("by ")(1).toInt
        val throwTrue::throwFalse::Nil = Seq(lines(4), lines(5)).map(l => l.split("monkey ")(1).toInt)
        val throwToMonkey: Item => Int = i => if (i % divisibleBy == 0) throwTrue else throwFalse
        part match {
            case PartOne => Monkey (items, divisibleBy, a => operation(a) / 3, throwToMonkey, 0)
            case PartTwo => Monkey (items, divisibleBy, a => operation(a), throwToMonkey, 0)
        }
    }

    @tailrec
    def round(state: State, maxRounds: Int): State =
        state match {
            case State(_, r) if r == maxRounds => state
            case State(monkeys, r) => round(
                State(monkeys.indices.foldLeft(monkeys)(monkeyAction), r + 1),
                maxRounds)
        }

    def monkeyAction(monkeys: Seq[Monkey], i: Int): Seq[Monkey] = {
        val monkey = monkeys(i)
        val itemsToThrow = monkey.items.map(
            item => {
                val worryLevel = monkey.worryLevel(item)
                (worryLevel, monkey.throwToMonkey(worryLevel))
            }
        )
        val thrownItemsByMonkey = itemsToThrow.groupBy(_._2)
        monkeys.zipWithIndex.map {
            case (m, _) if m == monkey => m.copy(items = Seq(), inspectedItems = m.inspectedItems + itemsToThrow.length)
            case (m, j) => m.copy(items = m.items ++ thrownItemsByMonkey.getOrElse(j, Seq.empty).map(_._1))
        }
    }

}
