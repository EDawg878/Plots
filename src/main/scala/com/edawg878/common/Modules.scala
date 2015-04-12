package com.edawg878.common

import com.edawg878.bukkit.commands.Basic.{SeenCommand, PlayTimeCommand}
import com.edawg878.bukkit.commands.Group.GroupCommand
import com.softwaremill.macwire.Macwire
import com.edawg878.common.Server._
import reactivemongo.api.MongoDriver

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Modules {

  trait CommonModule extends Macwire {
    lazy val logger = plugin.logger
    lazy val driver = new MongoDriver
    lazy val conn = driver.connection(List("localhost"))
    lazy val db = wire[MongoPlayerRepository]
    lazy val cdb = wire[MongoConfigRepository]

    def plugin: Plugin
    def server: Server
  }

  trait BukkitModule extends CommonModule {

    import com.edawg878.bukkit.BukkitConversions._
    import com.edawg878.bukkit.commands.Credit.CreditCommand
    import com.edawg878.bukkit.commands.Perk.PerkCommand
    import com.edawg878.bukkit.commands.Tier.TierCommand
    import org.bukkit.command.CommandExecutor

    lazy val tierCommand = wire[TierCommand]
    lazy val perkCommand = wire[PerkCommand]
    lazy val creditCommand = wire[CreditCommand]
    lazy val groupCommand = wire[GroupCommand]
    lazy val playTimeCommand = wire[PlayTimeCommand]
    lazy val seenCommand = wire[SeenCommand]

    override def plugin: Plugin = bukkitPlugin.toPlugin
    override def server: Server = bukkitPlugin.getServer.toServer

    def bukkitPlugin: org.bukkit.plugin.Plugin

  }

  trait BungeeModule extends CommonModule {

    import com.edawg878.bungee.BungeeConversions._

    lazy val server: Server.Server = bungeePlugin.getProxy.toServer

    override def plugin: Plugin = bungeePlugin.toPlugin

    def bungeePlugin: net.md_5.bungee.api.plugin.Plugin

  }
}