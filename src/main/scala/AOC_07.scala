import scala.io.Source

object AOC_07 {

    case class Model(currentNode: Dir, parents: Seq[Dir])

    trait Command
    case class ChangeDir(toDir: String) extends Command
    case object ChangeDirParent extends Command
    case object ChangeDirRoot extends Command
    case class ListDir(results: Seq[Result]) extends Command

    trait Result{ def name: String }
    case class ResultDir(name: String) extends Result
    case class ResultFile(name: String, size: Int) extends Result

    sealed trait Node {
        def name: String
        def size: Int
    }
    case class Dir(name: String, children: Seq[Node], size: Int) extends Node
    case class File(name: String, size: Int) extends Node

    def main(args: Array[String]): Unit = {
        System.out.println(
            systemSize(Source.fromResource("day7_input.txt"))
        )
    }

    def systemSize(source: Source): Int = {
        val model = source.getLines()
          .foldLeft(Seq[Command]())(parseInput).reverse
          .foldLeft(Model(Dir("/", Seq(), 0), Seq()))(populateTree)

        val root: Dir = model.parents.lastOption.getOrElse(model.currentNode)

        // part one
        findDirectories(root)
          .map(_.size)
          .filter(_ < 100000)
          .sum

        // part two
        findDirectories(root)
          .map(_.size)
          .filter(_ > 30000000 - 70000000 + root.size)
          .min
    }

    def parseInput(commands: Seq[Command], line: String): Seq[Command] =
        (line.split(" ").toSeq, commands) match {
            case (Seq("$", "cd", "/"), c) => ChangeDirRoot +: c
            case (Seq("$", "cd", ".."), c) => ChangeDirParent +: c
            case (Seq("$", "cd", toDir), c) => ChangeDir(toDir) +: c
            case (Seq("$", "ls"), c) => ListDir(Seq()) +: c
            case (Seq("dir", dirName), ListDir(results)::cs) => ListDir(results :+ ResultDir(dirName)) +: cs
            case (Seq(size, fileName), ListDir(results)::cs) => ListDir(results :+ ResultFile(fileName, size.toInt))+: cs
        }

    def populateTree(model: Model, command: Command): Model =
        (command, model.currentNode, model.parents) match {
            case (ChangeDirRoot, currentNode, _) => Model(model.parents.lastOption.getOrElse(currentNode), Seq())
            case (ChangeDirParent, _, p::ps) => Model(p, ps)
            case (ChangeDir(dir), currentNode: Dir, parents) => Model(
                currentNode.children.collect{case d: Dir if d.name == dir => d}.head,
                currentNode +: parents
            )
            case (ListDir(results), currentNode, parents) =>
                val dir = addResults(currentNode, results)
                Model(dir, amendNodesParents(dir, parents, dir.size))
        }

    def addResults(node: Node, results: Seq[Result]): Dir = {
        val children: Seq[Node] = results.map {
            case d:ResultDir => Dir(d.name, Seq(), 0)
            case f:ResultFile => File(f.name, f.size)
        }
        Dir(node.name, children, children.map(_.size).sum)
    }

    def amendNodesParents(dir: Dir, parents: Seq[Dir], increaseSize: Int): Seq[Dir] =
        parents match {
            case Nil => Seq()
            case p::ps =>
                val d = Dir(p.name, p.children.map {
                    case Dir(dir.name, _, _) => dir
                    case n => n
                }, p.size + increaseSize)
                d +: amendNodesParents(d, ps, increaseSize)
        }

    def findDirectories(node: Node): Seq[Dir] =
        node match {
            case dir: Dir => dir +: dir.children.flatMap(findDirectories)
            case _ => Seq()
        }

}
