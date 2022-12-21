import scala.annotation.tailrec
import scala.io.Source

object AOC_21 {

    def main(args: Array[String]): Unit =
        Seq(
            monkeyAnswer(Source.fromResource("day21_sample.txt")),
            monkeyAnswer(Source.fromResource("day21_input.txt")),
            monkeyAnswerPartTwo(Source.fromResource("day21_sample.txt")),
            monkeyAnswerPartTwo(Source.fromResource("day21_input.txt")),
        ).foreach(println)

    sealed trait Op
    case object Plus extends Op
    case object Minus extends Op
    case object Times extends Op
    case object By extends Op

    type MonkeyName = String
    val Root: MonkeyName = "root"
    val Human: MonkeyName = "humn"

    sealed trait Monkey
    case class MonkeyAnswer(result: Option[Long]) extends Monkey
    case class MonkeyQuestion(op: Op, monkey1: MonkeyName, monkey2: MonkeyName) extends Monkey

    def monkeyAnswer(source: Source): Long = {
        val monkeys: Map[String, Monkey] = source.getLines().map(lineToMonkey).toMap
        findAnswer(monkeys, List(Root: MonkeyName))(Root).asInstanceOf[MonkeyAnswer].result.get
    }

    def monkeyAnswerPartTwo(source: Source): Long = {
        val monkeys: Map[String, Monkey] = source.getLines().map(lineToMonkeyPartTwo).toMap
        val solutions: Map[String, Monkey] = findAnswer(monkeys, List(Root: MonkeyName))

        val rootQuestion: MonkeyQuestion = monkeys(Root).asInstanceOf[MonkeyQuestion]
        findHumanValue(monkeys, solutions, rootQuestion, 0, Root)
    }

    implicit def strToOp(str: String): Op = str match {
        case "+" => Plus
        case "-" => Minus
        case "*" => Times
        case "/" => By
    }

    def lineToMonkey(line: String): (String, Monkey) =
        line.split(": ") match {
            case Array(monkeyName, segment) =>segment.split(" ") match {
                case Array(m1, op, m2) => monkeyName -> MonkeyQuestion(op, m1, m2)
                case Array(m) => monkeyName -> MonkeyAnswer(Some(m.toInt))
            }
        }

    def lineToMonkeyPartTwo(line: String): (String, Monkey) =
        line.split(": ") match {
            case Array(Human, _) => (Human, MonkeyAnswer(None))
            case Array(monkeyName, segment) =>segment.split(" ") match {
                case Array(m1, op, m2) => monkeyName -> MonkeyQuestion(op, m1, m2)
                case Array(m) => monkeyName -> MonkeyAnswer(Some(m.toInt))
            }
        }

    @tailrec
    def findAnswer(monkeys: Map[String, Monkey], pendingResults: List[MonkeyName]): Map[String, Monkey] =
        pendingResults match {
            case Nil => monkeys
            case monkeyName::ms => answerQuestion(monkeys, monkeyName) match {
                case Right(answer) => findAnswer(monkeys.updated(monkeyName, MonkeyAnswer(answer)), ms)
                case Left(questions) => findAnswer(monkeys, questions ++ pendingResults)
            }
        }

    def calculate(op: Op, r1: Long, r2: Long): Long =
        op match {
            case Plus => r1 + r2
            case Minus => r1 -r2
            case Times => r1 * r2
            case By => r1 / r2
        }

    def answerQuestion(monkeys: Map[String, Monkey], monkeyName: MonkeyName): Either[List[MonkeyName], Option[Long]] = {
        val question = monkeys(monkeyName).asInstanceOf[MonkeyQuestion]
        (monkeys(question.monkey1), monkeys(question.monkey2)) match {
            case (MonkeyAnswer(None), _) | (_, MonkeyAnswer(None)) => Right(None)
            case (_: MonkeyQuestion, _: MonkeyQuestion) => Left(List(question.monkey1, question.monkey2))
            case (_: MonkeyQuestion, _: MonkeyAnswer) => Left(List(question.monkey1))
            case (_: MonkeyAnswer, _: MonkeyQuestion) => Left(List(question.monkey2))
            case (MonkeyAnswer(Some(r1)), MonkeyAnswer(Some(r2))) => Right(Some(calculate(question.op, r1, r2)))
        }
    }

    @tailrec
    def findHumanValue(monkeys: Map[String, Monkey], solutions: Map[String, Monkey], monkey: Monkey, acc: Long, monkeyName: MonkeyName): Long =
        (monkeyName, monkey) match {
            case (Human, _) => acc
            case (Root, MonkeyQuestion(op, m1, m2)) =>
                (op, solutions(m1), solutions(m2)) match {
                    case (Plus | Minus, MonkeyAnswer(Some(m1)), MonkeyAnswer(None)) => findHumanValue(monkeys, solutions, monkeys(m2), m1, m2)
                    case (Plus | Minus, MonkeyAnswer(None), MonkeyAnswer(Some(m2))) => findHumanValue(monkeys, solutions, monkeys(m1), m2, m1)
                    case (Times | By, MonkeyAnswer(Some(m1)), MonkeyAnswer(None)) => findHumanValue(monkeys, solutions, monkeys(m2), m1, m2)
                    case (Times | By, MonkeyAnswer(None), MonkeyAnswer(Some(m2))) => findHumanValue(monkeys, solutions, monkeys(m1), m2, m1)
                }
            case (_, _ @ MonkeyQuestion(op, monkey1, monkey2)) =>
                (op, solutions(monkey1), solutions(monkey2)) match {
                    case (op, MonkeyAnswer(Some(r1)), MonkeyAnswer(None)) =>
                        findHumanValue(monkeys, solutions, monkeys(monkey2), calculateOperand(op, Some(r1), None, acc), monkey2)
                    case (op, MonkeyAnswer(None), MonkeyAnswer(Some(r2))) =>
                        findHumanValue(monkeys, solutions, monkeys(monkey1), calculateOperand(op, None, Some(r2), acc), monkey1)
                    case (op, _: MonkeyQuestion, _) =>
                        val answersLeft = findAnswer(monkeys, List(monkey1))
                        val resultLeft = answersLeft(monkey1).asInstanceOf[MonkeyAnswer]
                        findHumanValue(monkeys, solutions ++ answersLeft, monkey, calculateOperand(op, resultLeft.result, None, acc), monkey2)
                    case (op, _, _: MonkeyQuestion) =>
                        val answersRight = findAnswer(monkeys, List(monkey2))
                        val resultRight = answersRight(monkey2).asInstanceOf[MonkeyAnswer]
                        findHumanValue(monkeys, solutions ++ answersRight, monkey, calculateOperand(op, None, resultRight.result, acc), monkey1)
                }
        }

    def calculateOperand(op: Op, l: Option[Long], r: Option[Long], acc: Long): Long =
        (op, l, r) match {
            case (Plus, Some(r), None) => acc - r
            case (Plus, None, Some(r)) => acc - r
            case (Minus, Some(r), None) => r - acc
            case (Minus, None, Some(r)) => r + acc
            case (Times, Some(r), None) => acc / r
            case (Times, None, Some(r)) => acc / r
            case (By, Some(r), None) => r / acc
            case (By, None, Some(r)) => r * acc
        }
}
