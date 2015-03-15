package com.edawg878.common

import com.edawg878.common.MessageFormatter._

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Conversions {

  implicit class RichPlayerData(val self: PlayerData) extends AnyVal {

    def displayTier: String = info"[${self.name}] has tier [${self.counters.tier}]"

    def displayPerks: String = {
      if (self.perks.isEmpty) info"[${self.name}] has no perks"
      else info"[${self.name}] has the following perks: ${self.perks.mkStringPretty}"
    }

  }

  implicit class RichTraversable(val self: Traversable[_]) extends AnyVal {
    def mkStringPretty: String = self.mkString(MessageFormatter.SECONDARY,
      MessageFormatter.PRIMARY + ", " + MessageFormatter.SECONDARY,
      MessageFormatter.PRIMARY)
  }

  implicit class RichInt(val self: Int) extends AnyVal {
    def clamp(min: Int, max: Int): Int = {
      if (self > max) max
      else if (self < min) min
      else self
    }
  }

}
