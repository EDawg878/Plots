package com.edawg878.common

import com.edawg878.bukkit.commands.BasicCommand.{WhoIsCommand, SeenCommand, PlayTimeCommand}
import com.edawg878.bukkit.commands.CreditCommand.CreditCommand
import com.edawg878.bukkit.commands.GroupCommand.GroupCommand
import com.edawg878.bukkit.commands.PerkCommand.PerkCommand
import com.edawg878.bukkit.commands.PlotCommand.PlotCommand
import com.edawg878.bukkit.commands.TierCommand.TierCommand
import com.edawg878.bukkit.listener.PlotListener
import com.edawg878.bukkit.plot.PlotClearConversation.PlotClearConversation
import com.edawg878.bukkit.plot._
import org.bukkit.command.CommandSender
import org.bukkit.event.Listener
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import com.edawg878.common.Server._
import reactivemongo.api.{DB, MongoDriver}

import scala.concurrent.{Await, Future}

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Modules {

  trait CommonModule {
    val logger = plugin.logger
    val driver = new MongoDriver
    val conn = driver.connection(List("localhost"))
    val mongo: DB = conn.db("minecraft")
    val playerDb = new MongoPlayerRepository(mongo, logger)
    val plotDb = new MongoPlotRepository(mongo, logger)
    val databases = Seq[MongoRepository](playerDb, plotDb)

    def plugin: Plugin
    def server: Server
  }

  trait BukkitModule extends CommonModule {

    import com.edawg878.bukkit.BukkitConversions._

    val configFile = plugin.dataFolder.resolve("config.json")
    val plotWorldConfigFile = plugin.dataFolder.resolve("worlds.json")
    val plotWorldConfigs = Configuration.load(plugin, plotWorldConfigFile)
    val plotWorlds = loadPlotWorlds
    val playerCache = new PlayerCache

    def plugin: Plugin = bukkitPlugin.toPlugin
    def server: Server = bukkitServer.toServer(bukkitPlugin)
    def bukkitPlugin: org.bukkit.plugin.Plugin
    def bukkitServer: org.bukkit.Server = bukkitPlugin.getServer

    val bukkitPlotWorldResolver = new PlotWorldResolver {
      override def apply(s: String): Option[PlotWorld] = plotWorlds.get(s)
    }

    def getPlotWorldConfig(s: String): Option[PlotWorldConfig] = plotWorldConfigs.find(_.name == s)

    def loadPlotWorlds: Map[String, PlotWorld] = {
      val fMan = plotWorldConfigs.map(PlotWorld.load(_, plotDb))
      val fManSeq = Future.sequence(fMan)
      val r = Await.result(fManSeq, 5 minutes)
      r.map{ case w => w.config.name -> w }.toMap
    }

    val tierCommand = new TierCommand(playerDb)
    val perkCommand = new PerkCommand(playerDb)
    val creditCommand = new CreditCommand(playerDb)
    val groupCommand = new GroupCommand(playerDb)
    val playTimeCommand = new PlayTimeCommand(playerDb, server)
    val seenCommand = new SeenCommand(playerDb, server)
    val whoIsCommand = new WhoIsCommand(playerDb)
    val plotClearConversation = new PlotClearConversation(bukkitPlotWorldResolver, bukkitPlugin, plotDb, bukkitServer)
    val plotCommand = new PlotCommand(bukkitPlotWorldResolver, playerDb, plotDb, server, bukkitServer, plotClearConversation)

    val plotListener = new PlotListener(bukkitPlotWorldResolver, plotDb, server, bukkitServer)

    val commands = Seq[Command[CommandSender]](tierCommand, perkCommand, creditCommand, groupCommand, playTimeCommand,
      seenCommand, whoIsCommand, plotCommand)
    val listeners = Seq[Listener](plotListener)


  }
}