import scala.annotation.tailrec
import scala.io.Source

object AOC_05 {

    def main(args: Array[String]): Unit = {
        System.out.println(
            reorderCrates(Source.fromResource("day5_input.txt"))
        )
    }

    type Stack = List[Char]
    case class Movement(count: Int, from:Int, to: Int)
    case class Model(stacks: Seq[Stack], movements: Seq[Movement])
    type CraneSort = Stack => Stack
    val part1CraneSort: CraneSort = s => s
    val part2CraneSort: CraneSort = _.reverse

    def reorderCrates(source: Source): String = {
        val input = source.getLines().foldLeft(Model(Seq(), Seq()))(processLine)
        moveCrates(input, part1CraneSort).stacks.map(_.head).mkString
    }

    def processLine(input: Model, line: String): Model =
        line match {
            case l if l startsWith "move" => Model(input.stacks, input.movements :+ movementFromLine(line))
            case "" => input
            case l if l startsWith " 1" => input
            case _ => Model(addLineToStacks(line, input.stacks), input.movements)
        }

    def movementFromLine(line: String): Movement = {
        val lineNumbers = line.split(' ').zipWithIndex.filter(_._2 % 2 == 1).map(_._1.toInt)
        Movement(lineNumbers(0), lineNumbers(1) - 1, lineNumbers(2) - 1)
    }

    def addLineToStacks(line: String, stacks: Seq[Stack]): Seq[Stack] =
        line.grouped(4)
          .map(_.charAt(1)).zipWithIndex
          .map {
              case (' ', _) => List()
              case (c, i) if stacks.length <= i => List(c)
              case (c, i) => stacks(i) :+ c
          }.toSeq

    @tailrec
    def moveCrates(model: Model, craneSort: CraneSort): Model = model match {
        case Model(_, Nil) => model
        case Model(stacks, m::movements) =>
            val crates = stacks(m.from).take(m.count)
            val s = stacks.zipWithIndex.map {
                case (s, i) if i == m.to => craneSort(crates) ++ s
                case (s, i) if i == m.from => s.drop(m.count)
                case (s, _) => s
            }
            moveCrates(Model( s, movements), craneSort)
    }

}
