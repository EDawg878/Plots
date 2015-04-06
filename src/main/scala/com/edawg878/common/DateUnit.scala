package com.edawg878.common

import java.time._
import java.util.concurrent.TimeUnit

import scala.annotation.tailrec

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object DateUnit {

  sealed trait DateUnit {
    def singular: String
    def plural: String = singular + "s"
    def abbreviation: String
    val millis: Long
  }

  case object Years extends DateUnit {
    val singular: String = "year"
    val abbreviation: String = "yr"
    val millis: Long = TimeUnit.DAYS.toMillis(365)
  }

  case object Months extends DateUnit {
    val singular: String = "month"
    val abbreviation: String = "mo"
    val millis: Long = TimeUnit.DAYS.toMillis(30)
  }

  case object Days extends DateUnit {
    val singular: String = "day"
    val abbreviation: String = "d"
    val millis: Long = TimeUnit.DAYS.toMillis(1)
  }

  case object Hours extends DateUnit {
    val singular: String = "hour"
    val abbreviation: String = "h"
    val millis: Long = TimeUnit.HOURS.toMillis(1)
  }

  case object Minutes extends DateUnit {
    val singular: String = "minute"
    val abbreviation: String = "m"
    val millis: Long = TimeUnit.MINUTES.toMillis(1)
  }

  case object Seconds extends DateUnit {
    val singular: String = "second"
    val abbreviation: String = "s"
    val millis: Long = TimeUnit.SECONDS.toMinutes(1)
  }

  object Implicits {
    // Units should be sorted descending
    implicit val preciseUnits: Seq[DateUnit] = Vector(Years, Months, Days, Hours, Minutes, Seconds)
    implicit val standardUnits: Seq[DateUnit] = Vector(Years, Months, Days, Hours, Minutes)
    implicit val defaultZone: ZoneId = ZoneId.of("America/Los_Angeles")
    implicit class RichInstant(time: Instant) {
      def toLocalDate(implicit zone: ZoneId): LocalDate = time.atZone(zone).toLocalDate
      def toLocalDateTime(implicit zone: ZoneId): LocalDateTime = time.atZone(zone).toLocalDateTime
      def toLocalTime(implicit zone: ZoneId): LocalTime = time.atZone(zone).toLocalTime
    }
  }

  def format(duration: Duration, abbreviate: Boolean = false)(implicit units: Seq[DateUnit]): Seq[String] = {
    require (units.nonEmpty)
    def fmt(num: Long, unit: DateUnit): String = {
      val name = if (num == 1) unit.singular else unit.plural
      num + " " + name
    }
    @tailrec def loop(i: Int = 0, millis: Long, seq: Seq[String]): Seq[String] = {
      if (i == units.length) seq
      else {
        val unit = units(i)
        val num = millis / unit.millis
        val remaining = millis % unit.millis
        if (num > 0) loop(i + 1, remaining, seq :+ fmt(num, unit))
        else loop(i + 1, remaining, seq)
      }
    }
    loop(millis = math.abs(duration.toMillis), seq = Vector()) match {
      case Nil =>
        if (abbreviate) Seq("<1 " + units.last.abbreviation)
        else Seq("less than 1 " + units.last.singular)
      case seq => seq
    }
  }

}
