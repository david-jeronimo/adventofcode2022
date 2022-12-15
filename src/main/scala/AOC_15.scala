import scala.annotation.tailrec
import scala.io.Source
import scala.math.{abs, max}

object AOC_15 {

    def main(args: Array[String]): Unit = {
        Seq(
            uncoveredPositions(Source.fromResource("day15_sample.txt"), 10),
            uncoveredPositions(Source.fromResource("day15_input.txt"), 2_000_000),
            findDistressBeacon(Source.fromResource("day15_sample.txt"), 20),
            findDistressBeacon(Source.fromResource("day15_input.txt"), 4_000_000),
        ).foreach(println)
    }

    case class Position(x: Long, y: Long)
    case class Sample(sensor: Position, beacon: Position)
    case class CoveredRange(from: Long, to: Long)

    def uncoveredPositions(source: Source, rowNumber: Long): Long = {
        val samples = source.getLines().map(lineToSample).toSeq
        val coveredRanges = samples.foldLeft(List[CoveredRange]())((acc, sample) => rowCoveredPositions(sample, acc, rowNumber)).sortBy(_.from)
        val mergedCoverRanges = mergeCoveredRanges(coveredRanges, List())
        val beaconPositions = samples.map(_.beacon).filter(_.y == rowNumber).map(_.x).distinct
        mergedCoverRanges.map(cr => cr.to - cr.from + 1).sum - beaconPositions.length

    }

    def findDistressBeacon(source: Source, limit: Int): Long = {
        val samples = source.getLines().map(lineToSample).toSeq
        (0 to limit).map(rowNumber => {
                val cr = samples.foldLeft(List[CoveredRange]())((acc, sample) => rowCoveredPositions(sample, acc, rowNumber)).sortBy(_.from)
                mergeCoveredRanges(cr, List())
        })
          .map(coveredRanges => findGap(coveredRanges, 0, limit))
          .zipWithIndex
          .find{
              case (Some(_), _) => true
              case _ => false
          }.map({
            case (Some(cr), i) => cr.from * 4_000_000 + i
        }).head
    }

    def lineToSample(line: String): Sample =
        line.split(":").map(
            halfLine => halfLine.split(",").map(
                coord => coord.split("=")(1).toInt
            ) match {
                case Array(x, y) => Position(x, y)
            }
        ) match {
            case Array(p1, p2) => Sample(p1, p2)
        }

    def rowCoveredPositions(sample: Sample, result: List[CoveredRange], rowNumber: Long): List[CoveredRange] = {
        val sampleReach = distance(sample.sensor, sample.beacon)
        val sampleToRowDistance = abs(sample.sensor.y - rowNumber)
        val numCoveredPositionsInRow = sampleReach - sampleToRowDistance
        if (numCoveredPositionsInRow > 0) {
            val coveredRange = CoveredRange(sample.sensor.x - numCoveredPositionsInRow, sample.sensor.x + numCoveredPositionsInRow)
            coveredRange +: result
        } else result
    }

    def distance(p1: Position, p2: Position): Long =
        abs(p1.x - p2.x) + abs(p1.y - p2.y)

    def mergeCoveredRanges(coveredRanges: List[CoveredRange], result: List[CoveredRange]): List[CoveredRange] =
        coveredRanges match {
            case _ :: Nil => result
            case cr1 :: cr2 :: crs => mergeCoveredRange(cr1, cr2) match {
                case l@m :: Nil => mergeCoveredRanges(m +: crs, l).distinct
                case l@m :: _ => m +: mergeCoveredRanges(crs, l).distinct
            }
        }

    def mergeCoveredRange(range1: CoveredRange, range2: CoveredRange): List[CoveredRange] =
        (range1, range2) match {
            case (r1, r2) if r1.from <= r2.from && r1.to >= r2.to => List(range1)
            case (r1, r2) if r2.from <= r1.from && r2.to >= r1.to => List(range2)
            case (r1, r2) if r1.from <= r2.from && r1.to + 1 >= r2.from => List(CoveredRange(r1.from, max(r1.to, r2.to)))
            case (r1, r2) if r2.from <= r1.from && r2.to + 1>= r1.from => List(CoveredRange(r2.from, max(r1.to, r2.to)))
            case (r1, r2) => List(r1, r2)
        }

    @tailrec
    def findGap(ranges: List[CoveredRange], from: Long, limit: Long): Option[CoveredRange] =
        ranges match {
            case Nil => None
            case _ if from > limit => None
            case r::rs if r.from > from + 1 => Some(CoveredRange(from + 1, r.from))
            case r::rs => findGap(rs, r.to, limit)
        }

}
