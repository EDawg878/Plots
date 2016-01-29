package com.edawg878.common

import java.time.{Duration, Instant}
import java.util.UUID

import com.edawg878.common.Color.Formatter
import com.edawg878.common.DateUnit.Implicits.standardUnits
import com.edawg878.common.DateUnit.TimeZone
import com.edawg878.common.Group.Default
import com.edawg878.common.Server.Player

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
case class PlayTime(firstLogin: Instant = Instant.now,
                    lastSeen: Instant = Instant.now,
                    amount: Duration = Duration.ZERO) {

  def login: PlayTime = copy(lastSeen = Instant.now)

  def logout: PlayTime = copy(amount = duration(online = true))

  def duration(online: Boolean = false): Duration = {
    if (online) amount.plus(activity)
    else amount
  }

  def activity: Duration = Duration.between(lastSeen, Instant.now)

}

case class PlayerData(id: UUID,
                      name: String,
                      usernames: mutable.Set[String] = mutable.LinkedHashSet(),
                      displayName: Option[String] = None,
                      group: Group = Default,
                      perks: Set[String] = Set(),
                      tier: Int = 0,
                      plotLimit: Int = 1,
                      voteCredits: Int = 0,
                      playTime: PlayTime = PlayTime())
  extends Ordered[PlayerData] {

  def this(p: Player) = this(id = p.id, name = p.name)

  def login: PlayerData = copy(playTime = playTime.login)

  def logout: PlayerData = copy(playTime = playTime.logout)

  def updateName(s: String): PlayerData = {
    if (name == s) this
    else {
      val c = usernames.clone()
      c.remove(name)
      c.add(name)
      copy(name = s, usernames = c)
    }
  }

  def playTimeToString(online: Boolean): String = {
    val time = DateUnit.format(playTime.duration(online)).mkString(" ")
    val first = playTime.firstLogin.atZone(TimeZone.Default).toLocalDate.toString
    info"$name has played for $time since $first"
  }

  def tierToString: String = info"$name has tier $tier"

  def creditsToString: String = {
    if (voteCredits == 0) info"$name has no credits"
    else if (voteCredits == 1) info"$name has $voteCredits credit"
    else info"$name has $voteCredits credits"
  }

  def perksToString: String = {
    if (perks.isEmpty) info"$name has no perks"
    else info"$name has the following perks: ${perks.mkString(", ")}"
  }

  def groupToString: String = info"$name is in group ${group.name}"

  override def compare(that: PlayerData): Int = this.playTime.lastSeen compareTo that.playTime.lastSeen
}

class PlayerCache(players: TrieMap[UUID, PlayerData] = TrieMap()) {

  def update(d: PlayerData): Unit = players.put(d.id, d)

}