import scala.annotation.tailrec
import scala.io.Source

object AOC_13 {

    def main(args: Array[String]): Unit = {
        Seq(
            decodeSignal(Source.fromResource("day13_sample.txt")),
            decodeSignal(Source.fromResource("day13_input.txt")),
            decodeSignalPart2(Source.fromResource("day13_sample.txt")),
            decodeSignalPart2(Source.fromResource("day13_input.txt"))
        ).foreach(println)
    }

    sealed trait Value
    case class NodeN(v: Int) extends Value
    case class NodeL(v: List[Value], number: Int = 0) extends Value

    sealed trait Result
    case object Right extends Result
    case object Wrong extends Result
    case object Same extends Result

    case class Packet(left: Value, right: Value)
    val dividerPackets = Seq(
        NodeL(List(NodeN(2))),
        NodeL(List(NodeN(6)))
    )

    def decodeSignal(source: Source): Int =
        source.getLines().grouped(3).toSeq
          .map(g => linesToPacket(g))
          .map(packet =>
              isRightOrder(packet.left, packet.right))
          .zipWithIndex.filter(e => e._1 == Right)
          .map(_._2 + 1)
          .sum

    def decodeSignalPart2(source: Source): Int = {
        val packets = source.getLines().grouped(3).toSeq
          .map(g => linesToPacket(g))
          .flatMap(p => Seq(p.left, p.right)) ++ dividerPackets
        val sortedPackets = packets.sortWith(isRightOrder(_, _) == Right)
        dividerPackets.map(dp => sortedPackets.indexOf(dp) + 1).product
    }

    def linesToPacket(lines: Seq[String]): Packet =
        Packet(
            lineToNode(lines(0).toList, NodeL(List[Value]()), Nil, 1),
            lineToNode(lines(1).toList, NodeL(List[Value]()), Nil, 1)
        )

    @tailrec
    def lineToNode(line: List[Char], node: NodeL, parentNodes: Seq[NodeL], nodeCount: Int): Value = {
        def getInt(s: Char, ss: List[Char]): Int =
            (s +: ss).takeWhile(c => c != ',' && c != ']').mkString.toInt

        def addNodeToParents(parents: Seq[NodeL], node: NodeL): Seq[NodeL] =
            parents match {
                case Nil => Seq()
                case p::ps if p.v.exists {
                    case NodeL(_, n) => n == node.number
                    case _ => false
                } =>
                    val parent = NodeL(p.v.map {
                        case NodeL(_, number) if number == node.number => node
                        case n: Value => n
                    }, p.number)
                    parent +: addNodeToParents(ps, parent)
                case p :: ps =>
                    val parent = NodeL(p.v :+ node, p.number)
                    parent +: addNodeToParents(ps, parent)
            }

        line match {
            case Nil => parentNodes.lastOption.getOrElse(node)
            case '['::ss => lineToNode(ss, NodeL(List[Value](), nodeCount + 1), node +: parentNodes, nodeCount + 1)
            case ']'::ss =>
                val parents = addNodeToParents(parentNodes, node)
                lineToNode(ss, parents.head, parents.drop(1), nodeCount)
            case ','::ss => lineToNode(ss, node, parentNodes, nodeCount)
            case s::ss =>
                getInt(s, ss) match {
                    case n if n > 9 => lineToNode(ss.drop(1), NodeL(node.v :+ NodeN(n), node.number), parentNodes, nodeCount)
                    case n => lineToNode(ss, NodeL(node.v :+ NodeN(n), node.number), parentNodes, nodeCount)
                }
        }
    }

    def isRightOrder(value1: Value, value2: Value): Result =
        (value1, value2) match {
            case (NodeN(a), NodeN(b)) if a == b => Same
            case (NodeN(a), NodeN(b)) if a < b => Right
            case (NodeN(a), NodeN(b)) if a > b => Wrong
            case (NodeL(a, _), NodeL(b, _)) => a.zip(b).map(v => isRightOrder(v._1, v._2)).find(_ != Same).getOrElse(
                (a.length, b.length) match {
                    case (l1, l2) if l1 > l2 => Wrong
                    case (l1, l2) if l1 < l2 => Right
                    case _ => Same
                }
            )
            case (NodeL(Nil, _), _) => Right
            case (a: NodeN, b: NodeL) => isRightOrder(NodeL(List(a)), b)
            case (a: NodeL, b: NodeN) => isRightOrder(a, NodeL(List(b)))
        }
}
