package com.edawg878.common

import com.edawg878.common.MessageFormatter._

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Conversions {

  implicit class RichPlayerData(val self: PlayerData) extends AnyVal {

    private def name: String = self.name

    def displayTier: String = info"[$name] has tier [${self.counters.tier}]"

    def displayCredits: String = {
      val credits = self.counters.voteCredits
      if (credits == 0) info"[$name] has no credits"
      else if (credits == 1) info"[$name] has [1] credit"
      else info"[$name] has [$credits] credits"
    }

    def displayPerks: String = {
      val perks = self.perks
      if (perks.isEmpty) info"[$name] has no perks"
      else info"[$name] has the following perks: ${perks.mkStringPretty}"
    }

  }

  implicit class RichTraversable[A](val self: Traversable[A]) extends AnyVal {

    def mkStringPretty: String = self.mkString(MessageFormatter.SECONDARY,
      MessageFormatter.PRIMARY + ", " + MessageFormatter.SECONDARY,
      MessageFormatter.PRIMARY)

    def toOption: Option[Traversable[A]] = {
      if (self.isEmpty) None
      else Some(self)
    }

  }

  implicit class RichInt(val self: Int) extends AnyVal {

    def clamp(min: Int, max: Int): Int = {
      if (self > max) max
      else if (self < min) min
      else self
    }

  }

}
