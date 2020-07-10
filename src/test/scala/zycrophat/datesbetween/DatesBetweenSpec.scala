package zycrophat.datesbetween

import java.time.{LocalDate, Period, ZoneId, ZonedDateTime}

import DatesBetween._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class DatesBetweenSpec extends AnyFlatSpec with Matchers {

  private val daysOfMarch = for (day <- (1 until 31).inclusive) yield LocalDate.of(2020, 3, day)

  "Given two dates (both inclusive), datesBetween" should "return all dates between" in {
    val start = daysOfMarch.head
    val end = daysOfMarch.last

    val theDatesBetween = datesBetween(start.inclusive, end.inclusive)

    theDatesBetween should have size daysOfMarch.size
    theDatesBetween should equal (daysOfMarch)
  }

  "Given two dates (start inclusive, end exclusive), datesBetween" should "return all dates between except end" in {
    val start = daysOfMarch.head
    val end = daysOfMarch.last

    val theDatesBetween = datesBetween(start.inclusive, end.exclusive)

    theDatesBetween should have size (daysOfMarch.size - 1)
    theDatesBetween should equal (daysOfMarch.dropRight(1))
  }

  "Given two dates (start exclusive, end exclusive), datesBetween" should
    "return all dates between except start and end" in {
    val start = daysOfMarch.head
    val end = daysOfMarch.last

    val theDatesBetween = datesBetween(start.exclusive, end.exclusive)

    theDatesBetween should have size (daysOfMarch.size - 2)
    theDatesBetween should equal (daysOfMarch.tail.dropRight(1))
  }

  "Given two dates (start exclusive, end inclusive), datesBetween" should "return all dates between except start" in {
    val start = daysOfMarch.head
    val end = daysOfMarch.last

    val theDatesBetween = datesBetween(start.exclusive, end.inclusive)

    theDatesBetween should have size (daysOfMarch.size - 1)
    theDatesBetween should equal (daysOfMarch.tail)
  }

  "Given two dates, datesBetween" should "return all dates between" in {
    val start = daysOfMarch.head
    val end = daysOfMarch.last

    val theDatesBetween = datesBetween(start, end)

    theDatesBetween should have size daysOfMarch.size
    theDatesBetween should equal (daysOfMarch)
  }

  "Given a LocalDateTime and a LocalDate (start exclusive exclusive), datesBetween" should
    "return all dates between except the first two" in {
    val start = daysOfMarch.head.atStartOfDay()
    val end = daysOfMarch.last

    val theDatesBetween = datesBetween(start.exclusive.exclusive, end)

    theDatesBetween should have size (daysOfMarch.size - 2)
    theDatesBetween should equal (daysOfMarch.tail.tail)
  }

  "Given a two ZonedDateTimes (start exclusive) with an implicit conversion function, datesBetween" should
    "return all dates between except the first" in {
    val start = ZonedDateTime.of(daysOfMarch.head.atStartOfDay(), ZoneId.systemDefault())
    val end = ZonedDateTime.of(daysOfMarch.last.atStartOfDay(), ZoneId.systemDefault())

    implicit val zonedDateTimeToLocalDate: ZonedDateTime => LocalDate = (s: ZonedDateTime) => s.toLocalDate

    val theDatesBetween = datesBetween(start.exclusive, end.inclusive)

    theDatesBetween should have size (daysOfMarch.size - 1)
    theDatesBetween should equal (daysOfMarch.tail)
  }

  "Given two dates and a step of 2 days, datesBetween" should "return every second date between" in {
    val start = daysOfMarch.head
    val end = daysOfMarch.last

    val theDatesBetween = datesBetween(start.inclusive, end.inclusive, Period.ofDays(2))

    val everySecondDate = daysOfMarch.zipWithIndex.collect { case (e, i) if (i % 2) == 0 => e }
    theDatesBetween should have size everySecondDate.size
    theDatesBetween should equal (everySecondDate)
  }

  "Given two dates (with default bounds) and a step of 2 days, datesBetween" should
    "return every second date between" in {
    val start = daysOfMarch.head
    val end = daysOfMarch.last

    val theDatesBetween = datesBetween(start, end, Period.ofDays(2))

    val everySecondDate = daysOfMarch.zipWithIndex.collect { case (e, i) if (i % 2) == 0 => e }
    theDatesBetween should have size everySecondDate.size
    theDatesBetween should equal (everySecondDate)
  }

  "Given two dates and a step of 2 days, datesBetween using and" should "return every second date between" in {
    val start = daysOfMarch.head
    val end = daysOfMarch.last

    val theDatesBetween = datesBetween(start.exclusive and end.exclusive, Period.ofDays(2))

    val everySecondDateExclusive = daysOfMarch
      .drop(1)
      .dropRight(1)
      .zipWithIndex.collect { case (e, i) if (i % 2) == 0 => e }
    theDatesBetween should have size everySecondDateExclusive.size
    theDatesBetween should equal (everySecondDateExclusive)
  }

  "Given a a LocalDate and a ZonedDateTime with an implicit conversion function, datesBetween using and" should
    "return all dates between" in {
    val start = ZonedDateTime.of(daysOfMarch.head.atStartOfDay(), ZoneId.systemDefault())
    val end = daysOfMarch.last

    implicit val zonedDateTimeToLocalDate: ZonedDateTime => LocalDate = (s: ZonedDateTime) => s.toLocalDate

    val theDatesBetween = datesBetween(start and end)

    theDatesBetween should have size daysOfMarch.size
    theDatesBetween should equal (daysOfMarch)
  }
}
