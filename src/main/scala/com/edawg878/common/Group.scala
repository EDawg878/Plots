package com.edawg878.common

import net.md_5.bungee.api.ChatColor

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
sealed trait Group extends Ordered[Group] {
  val name: String
  val rank: Int
  val isStaff = false
  val color: ChatColor
  def prefix: String = s"[$name]"
  def node: String = s"group.${name.toLowerCase}"
  def compare(that: Group) = this.rank compareTo that.rank
}

object Group {

  val values: Seq[Group] = Seq(Default, Moderator, Admin, CoOwner, Owner).sorted(Ordering[Group])
  val valueMap: Map[String, Group] = values.map(g => g.name.toLowerCase -> g).toMap

  def withName(name: String): Option[Group] = valueMap.get(name.toLowerCase)

  def promote(group: Group): Group = values.find(_ > group) getOrElse group
  def demote(group: Group): Group = values.reverse.find(_ < group) getOrElse group

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

}