import scala.io.Source

object AOC_20 {

    def main(args: Array[String]): Unit =
        Seq(
            decrypt(Source.fromResource("day20_sample.txt")),
            decrypt(Source.fromResource("day20_input.txt")),
            decryptPartTwo(Source.fromResource("day20_sample.txt")),
            decryptPartTwo(Source.fromResource("day20_input.txt")),
        ).foreach(println)

    type Position = Int
    type Value = BigInt
    type Id = Int

    case class Item(value: Value, id: Id)

    def decrypt(source: Source): BigInt = {
        val original: List[Item] = source.getLines().map(_.toInt).zipWithIndex.map(e => Item(e._1, e._2)).toList
        val result: List[Item] = original.foldLeft(original)(mix)
        val positionZero = result.indexWhere(_.value == 0)
        List(
            result((positionZero + 1000) % result.length),
            result((positionZero + 2000) % result.length),
            result((positionZero + 3000) % result.length),
        ).map(_.value).sum
    }

    def decryptPartTwo(source: Source): BigInt = {
        val original: List[Item] = source.getLines().map(_.toInt).zipWithIndex.map(e => Item(e._1 * 811589153L, e._2)).toList
        val result = (0 until 10).foldLeft(original)(
            (prev, _) => original.foldLeft(prev)(mix)
        )
        val positionZero = result.indexWhere(_.value == 0)
        List(
            result((positionZero + 1000) % result.length),
            result((positionZero + 2000) % result.length),
            result((positionZero + 3000) % result.length),
        ).map(_.value).sum
    }

    def mix(list: List[Item], item: Item): List[Item] = {
        val positionFrom: Position = list.indexOf(item)
        val number = item.value
        val listAfterDropping = list.patch(positionFrom, Nil, 1)
        val positionTo: Position = number match {
            case n if n < 0 => (positionFrom + listAfterDropping.length + (n % listAfterDropping.length).toInt) % listAfterDropping.length match {
                case 0 => list.length - 1
                case p => p
            }
            case n => (positionFrom + (n % listAfterDropping.length).toInt) % listAfterDropping.length match {
                case 0 if positionFrom == 0 => 0
                case 0 => list.length - 1
                case p => p
            }
        }
        listAfterDropping.patch(positionTo, item :: Nil, 0)
    }

}
