package com.edawg878.common

import com.softwaremill.macwire.Macwire

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Modules {

  trait CommonModule extends Macwire {
    lazy val logger = plugin.getLogger
    lazy val db: MongoPlayerRepository = wire[MongoPlayerRepository]

    def plugin: Plugin
  }

  trait BukkitModule extends CommonModule {

    import com.edawg878.bukkit.BukkitImpl._
    import com.edawg878.bukkit.commands.Credit.CreditCommand
    import com.edawg878.bukkit.commands.Perk.PerkCommand
    import com.edawg878.bukkit.commands.Tier.TierCommand
    import org.bukkit.command.CommandExecutor

    lazy val server = bukkitPlugin.getServer

    lazy val tierCommand = wire[TierCommand].asInstanceOf[CommandExecutor]
    lazy val perkCommand = wire[PerkCommand].asInstanceOf[CommandExecutor]
    lazy val creditCommand = wire[CreditCommand].asInstanceOf[CommandExecutor]

    override def plugin = bukkitPlugin

    def bukkitPlugin: org.bukkit.plugin.Plugin

  }

  trait BungeeModule extends CommonModule {

    import com.edawg878.bungee.BungeeImpl._

    lazy val server = bungeePlugin.getProxy

    override def plugin = bungeePlugin

    def bungeePlugin: net.md_5.bungee.api.plugin.Plugin

  }
}