import scala.io.Source


case class Acc(countByElf: Seq[Int], currentCount: Int = 0)

object Application {

  def main(args: Array[String]): Unit = {
    System.out.println(
      maxCalories(Source.fromResource("day1_input.txt"), 3)
    )

  }

  def maxCalories(source: Source, numberOfBestElves: Int): Int = {

    implicit def lineValue(line: String): Option[Int] = line match {
      case "" => None
      case l => Some(l.toInt)
    }

    def addAndGetBestElves(best: Seq[Int], candidate: Int): Seq[Int] =
      (best :+ candidate).sortWith(_ > _).take(numberOfBestElves)

    def processLine(acc: Acc, line: String): Acc = (acc, line: Option[Int]) match {
      case (acc, None) => Acc(addAndGetBestElves(acc.countByElf, acc.currentCount))
      case (acc, Some(v)) => Acc(acc.countByElf, acc.currentCount + v)
    }

    val acc = source.getLines().foldLeft(Acc(Seq()))(processLine)
    addAndGetBestElves(acc.countByElf, acc.currentCount).sum
  }
}


