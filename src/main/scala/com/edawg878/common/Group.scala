package com.edawg878.common

import net.md_5.bungee.api.ChatColor

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Group {

  sealed trait Group extends Ordered[Group] {
    val name: String
    val rank: Int
    val isStaff = false
    val color: ChatColor
    def prefix = s"[$name]"
    def node = s"group.${name.toLowerCase}"
    def promote: Group = values.find(_ > this) getOrElse this
    def demote: Group = values.reverse.find(_ < this) getOrElse this
    def compare(that: Group) = this.rank compareTo that.rank
  }

  def withName(name: String, ignoreCase: Boolean = false): Option[Group] = {
    values.find(group =>
      if (ignoreCase) group.name.toLowerCase == name.toLowerCase
      else group.name == name
    )
  }

  trait Staff extends Group {
    override val isStaff = true
  }

  case object Default extends Group {
    val name = "Default"
    val rank = 0
    val color = ChatColor.WHITE
  }

  case object Moderator extends Staff {
    val name = "Moderator"
    val rank = 10
    val color = ChatColor.RED
  }

  case object Admin extends Staff {
    val name = "Admin"
    val rank = 30
    val color = ChatColor.GOLD
  }

  case object CoOwner extends Staff {
    val name = "Co-Owner"
    val rank = 50
    val color = ChatColor.GOLD
  }

  case object Owner extends Staff {
    val name = "Owner"
    val rank = 100
    val color = ChatColor.GOLD
  }

  var values = Vector(Default, Moderator, Admin, CoOwner, Owner).sorted(Ordering[Group])
}