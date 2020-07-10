package zycrophat.datesbetween

import java.time.{LocalDate, LocalDateTime, Period}


object DatesBetween {

  private val defaultStep = Period.ofDays(1)

  trait ToIntervalBounds {
    def inclusive: IntervalBounds
    def exclusive: IntervalBounds
    def and(x: ToIntervalBounds): IntervalBounds
  }

  def datesBetween(startEnd: IntervalBounds, step: Period = defaultStep): Seq[LocalDate] =
    datesBetween(startEnd.start, startEnd.end, step)

  def datesBetween(start: ToIntervalBounds, end: ToIntervalBounds): Seq[LocalDate] =
    datesBetween(start.inclusive, end.inclusive)

  def datesBetween(start: ToIntervalBounds, end: ToIntervalBounds, step: Period): Seq[LocalDate] =
    datesBetween(start.inclusive, end.inclusive, step)

  def datesBetween(lowerBound: IntervalBounds, upperBound: IntervalBounds): Seq[LocalDate] =
    datesBetween(lowerBound, upperBound, defaultStep)

  def datesBetween(lowerBound: IntervalBounds, upperBound: IntervalBounds, step: Period): Seq[LocalDate] =
    Iterator.iterate(lowerBound.start)(_.plus(step)).takeWhile(!_.isAfter(upperBound.end)).toSeq

  case class IntervalBounds(
    start: LocalDate,
    end: LocalDate
  )

  implicit class LocalDateToIntervalBound(localDate: LocalDate) extends ToIntervalBounds {
    override def inclusive: IntervalBounds = IntervalBounds(localDate, localDate)
    override def exclusive: IntervalBounds = IntervalBounds(localDate, localDate).exclusive

    override def and(x: ToIntervalBounds): IntervalBounds = IntervalBounds(localDate, x.inclusive.end)
  }

  implicit val localDateTimeToLocalDate: LocalDateTime => LocalDate =
    (localDateTime: LocalDateTime) => localDateTime.toLocalDate

  implicit class LocalDateSourceToIntervalBound[T](localDateSource: T)
                                                  (implicit toLocalDate: T => LocalDate)
    extends ToIntervalBounds {
    override def inclusive: IntervalBounds = toLocalDate(localDateSource).inclusive
    override def exclusive: IntervalBounds = toLocalDate(localDateSource).exclusive

    override def and(x: ToIntervalBounds): IntervalBounds = IntervalBounds(toLocalDate(localDateSource), x.inclusive.end)
  }

  implicit class IntervalBoundToIntervalBound(intervalBound: IntervalBounds) extends ToIntervalBounds {
    override def inclusive: IntervalBounds = intervalBound
    override def exclusive: IntervalBounds = IntervalBounds(intervalBound.start.plusDays(1), intervalBound.end.minusDays(1))

    override def and(x: ToIntervalBounds): IntervalBounds = IntervalBounds(intervalBound.start, x.inclusive.end)
  }

}
