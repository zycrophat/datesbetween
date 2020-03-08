package steffan.datesbetween

import java.time.{LocalDate, LocalDateTime, Period}


object DatesBetween {

  trait ToIntervalBounds {
    def inclusive: IntervalBound
    def exclusive: IntervalBound
  }

  def datesBetween(start: ToIntervalBounds, end: ToIntervalBounds): Seq[LocalDate] =
    datesBetween(start.inclusive, end.inclusive)

  def datesBetween(start: ToIntervalBounds, end: ToIntervalBounds, step: Period): Seq[LocalDate] =
    datesBetween(start.inclusive, end.inclusive, step)

  def datesBetween(lowerBound: IntervalBound, upperBound: IntervalBound): Seq[LocalDate] =
    datesBetween(lowerBound, upperBound, Period.ofDays(1))

  def datesBetween(lowerBound: IntervalBound, upperBound: IntervalBound, step: Period): Seq[LocalDate] =
    Iterator.iterate(lowerBound.start)(_.plus(step)).takeWhile(!_.isAfter(upperBound.end)).toSeq

  case class IntervalBound(
    start: LocalDate,
    end: LocalDate
  )

  implicit class LocalDateToIntervalBound(localDate: LocalDate) extends ToIntervalBounds {
    override def inclusive: IntervalBound = IntervalBound(localDate, localDate)
    override def exclusive: IntervalBound = IntervalBound(localDate, localDate).exclusive
  }

  implicit val localDateTimeToLocalDate: LocalDateTime => LocalDate =
    (localDateTime: LocalDateTime) => localDateTime.toLocalDate

  implicit class LocalDateSourceToIntervalBound[T](localDateSource: T)
                                                  (implicit toLocalDate: T => LocalDate)
    extends ToIntervalBounds {
    override def inclusive: IntervalBound = toLocalDate(localDateSource).inclusive
    override def exclusive: IntervalBound = toLocalDate(localDateSource).exclusive
  }

  implicit class IntervalBoundToIntervalBound(intervalBound: IntervalBound) extends ToIntervalBounds {
    override def inclusive: IntervalBound = intervalBound
    override def exclusive: IntervalBound = IntervalBound(intervalBound.start.plusDays(1), intervalBound.end.minusDays(1))
  }

}
