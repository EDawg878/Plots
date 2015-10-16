package com.edawg878.common

import java.time._
import java.util.concurrent.TimeUnit

import scala.annotation.tailrec

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object DateUnit {

  val Years = new DateUnit {
    val singular = "year"
    val abbreviation = "yr"
    val millis  = TimeUnit.DAYS.toMillis(365)
  }

  val Months = new DateUnit {
    val singular = "month"
    val abbreviation = "mo"
    val millis = TimeUnit.DAYS.toMillis(30)
  }

  val Days = new DateUnit {
    val singular = "day"
    val abbreviation = "d"
    val millis = TimeUnit.DAYS.toMillis(1)
  }

  val Hours = new DateUnit {
    val singular = "hour"
    val abbreviation = "h"
    val millis = TimeUnit.HOURS.toMillis(1)
  }

  val Minutes = new DateUnit {
    val singular = "minute"
    val abbreviation = "m"
    val millis = TimeUnit.MINUTES.toMillis(1)
  }

  val Seconds = new DateUnit {
    val singular = "second"
    val abbreviation = "s"
    val millis = TimeUnit.SECONDS.toMinutes(1)
  }

  sealed trait DateUnit {
    def singular: String
    def plural: String = singular + "s"
    def abbreviation: String
    val millis: Long
  }

  object Implicits {
    // Units should be sorted descending
    implicit val preciseUnits: Seq[DateUnit] = Vector(Years, Months, Days, Hours, Minutes, Seconds)
    implicit val standardUnits: Seq[DateUnit] = Vector(Years, Months, Days, Hours, Minutes)
  }

  object TimeZone {
    val Default: ZoneId = ZoneId.of("America/Los_Angeles")
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
