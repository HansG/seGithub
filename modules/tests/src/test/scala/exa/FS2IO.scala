package exa

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2._
import fs2.io.file._


trait FileReader {
  def description: String
  def consume(path: Path): Result
}


object FS2IO extends FileReader {
  override def consume(path: Path): Result =
    Files[IO]
          .readAll(path) //,EC  4096,
          .through(text.utf8.decode)
          .through(text.lines)
      .filter(_.nonEmpty)
      .compile
      .fold(LineMetricsAccumulator.empty)(_ addLine _)
      .map(_.asResult)
      .unsafeRunSync()
    //    implicit val executionContext: ExecutionContext = ExecutionContext.global
    //    implicit val readingExecutionContext =
    //      Resource.make(IO(ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))))(ec => IO(ec.shutdown()))

    //    Stream
    //      .resource(readingExecutionContext)
    //      .flatMap { EC =>
    //      }

  override def description: String = "fs2-io"
}





final case class Result(
                         lineCount: Int,
                         specialNames: List[(Int, FullName)],
                         donationsByMonth: Map[DateBucket, Int],
                         mostCommonFirstName: FirstName,
                         mostCommonFirstNameCount: Int
                       )

object Record {
  def unapply(line: String): Option[(FullName, Option[FirstName], DateBucket)] = {
    val grabber = line.split('|').lift
    for {
      fullName <- grabber(7).map(FullName)
      rawDate  <- grabber(4)
    } yield (fullName, FirstName(fullName), DateBucket(rawDate))
  }
}
/**
 * Holds the values needed to accumulate the results so far while processing the lines in the file.
 *
 * @param lineCount          number of lines before this one
 * @param specialNames       recorded names, and the indexes on which they were found - see
 *                           [livongo.large_files_blog_post.common.LineMetricsAccumulator#SpecialLineNames
 * @param donationsByMonth   the number of donations in a given month
 * @param firstNameFrequency the number of times each first name appears
 */
final class LineMetricsAccumulator(
                                    lineCount:          Int,
                                    specialNames:       List[(Int, FullName)],
                                    donationsByMonth:   Map[DateBucket, Int],
                                    firstNameFrequency: Map[FirstName, Int]
                                  ) {
   /* Progresses and incorporates the results of an additional line.
   * This logic is common across the various ways of reading the file, so it's abstracted out here.
   * @param line the raw line
   */
  def addLine(line: String): LineMetricsAccumulator = line match {
    case Record(fullName, firstNameOpt, dateBucket) =>
      val lineNumber = lineCount + 1
      new LineMetricsAccumulator(
        lineCount = lineNumber,
        specialNames =
          if (LineMetricsAccumulator.LineNumbersOfSpecialNames.contains(lineNumber))
            (lineNumber, fullName) :: specialNames
          else specialNames,
        donationsByMonth = donationsByMonth.updated(
          dateBucket,
          donationsByMonth.getOrElse(dateBucket, 0) + 1
        ),
        firstNameFrequency = firstNameOpt.fold(firstNameFrequency) { firstName =>
          firstNameFrequency.updated(
            firstName,
            firstNameFrequency.getOrElse(firstName, 0) + 1
          )
        }
      )
  }

  /**
   * Convert to [livongo.large_files_blog_post.common.Result by running the only aggregation (most common first name)
   */
  def asResult: Result = {
    val (mostCommonFirstName, mostCommonFirstNameCount) =
    // This orders first by frequency, then alphabetically to break any ties.
      firstNameFrequency.maxBy(_.swap)
    Result(
      lineCount,
      specialNames,
      donationsByMonth,
      mostCommonFirstName,
      mostCommonFirstNameCount
    )
  }
}

object LineMetricsAccumulator {
  def empty: LineMetricsAccumulator = new LineMetricsAccumulator(-1, List.empty, Map.empty, Map.empty)
  val LineNumbersOfSpecialNames = Set(0, 432, 43243)
}

final case class FirstName(value: String) extends AnyVal
object FirstName {
  /**
   * Extract the first name from a full name.
   * @param fullName the full name, which is expected to be in the format `family, given` or `family, given middle`
   * @return [[scala.None]] if the format isn't what we expect, or [[scala.Some]] if we can pull a
   *         [livongo.large_files_blog_post.common.FirstName]] out of the input.
   */
  def apply(fullName: FullName): Option[FirstName] = {
    for {
      firstAndMiddleNames <- fullName.value.split(',').tail.headOption.map(_.trim)
      if firstAndMiddleNames.nonEmpty
      firstName <- firstAndMiddleNames.split(' ').headOption
    } yield FirstName(firstName)
  }

  implicit val ordering: Ordering[FirstName] = Ordering.by(_.value)
}

final case class DateBucket(year: Int, month: Int) {
  override def toString: String = "%04d-%02d".format(year, month)
}
object DateBucket {

  /**
   * Companion object constructors are a common pattern in Scala, and a good place
   * for really common initialization code.
   *
   * @param raw a string, with the expected format 'YYYYMM' (longer is OK, shorter will break things)
   */
  def apply(raw: String): DateBucket = {
    val year  = raw.take(4).toInt
    val month = raw.slice(4, 6).toInt
    DateBucket(year, month)
  }

  implicit val ordering: Ordering[DateBucket] = Ordering.by(db => db.year -> db.month)
}

case class FullName(value : String)