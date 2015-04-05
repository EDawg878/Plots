package com.edawg878.common

import com.edawg878.bukkit.commands.Group.GroupCommand
import com.edawg878.bukkit.commands.PlayTime.PlayTimeCommand
import com.softwaremill.macwire.Macwire
import com.edawg878.common.Server._

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Modules {

  trait CommonModule extends Macwire {
    lazy val logger = plugin.getLogger
    lazy val db: MongoPlayerRepository = wire[MongoPlayerRepository]

    def plugin: Plugin
    def server: Server
  }

  trait BukkitModule extends CommonModule {

    import com.edawg878.bukkit.BukkitImpl._
    import com.edawg878.bukkit.commands.Credit.CreditCommand
    import com.edawg878.bukkit.commands.Perk.PerkCommand
    import com.edawg878.bukkit.commands.Tier.TierCommand
    import org.bukkit.command.CommandExecutor

    lazy val tierCommand = wire[TierCommand].asInstanceOf[CommandExecutor]
    lazy val perkCommand = wire[PerkCommand].asInstanceOf[CommandExecutor]
    lazy val creditCommand = wire[CreditCommand].asInstanceOf[CommandExecutor]
    lazy val groupCommand = wire[GroupCommand].asInstanceOf[CommandExecutor]
    lazy val playTimeCommand = wire[PlayTimeCommand].asInstanceOf[CommandExecutor]

    override def plugin: Plugin = bukkitPlugin.toPlugin
    override def server: Server = bukkitPlugin.getServer.toServer

    def bukkitPlugin: org.bukkit.plugin.Plugin

  }

  trait BungeeModule extends CommonModule {

    import com.edawg878.bungee.BungeeImpl._

    lazy val server: Server.Server = bungeePlugin.getProxy.toServer

    override def plugin: Plugin = bungeePlugin.toPlugin

    def bungeePlugin: net.md_5.bungee.api.plugin.Plugin

  }
}