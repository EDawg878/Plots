package com.edawg878.common

import com.edawg878.bukkit.commands.BasicCommand.{WhoIsCommand, SeenCommand, PlayTimeCommand}
import com.edawg878.bukkit.commands.CreditCommand.CreditCommand
import com.edawg878.bukkit.commands.GroupCommand.GroupCommand
import com.edawg878.bukkit.commands.PerkCommand.PerkCommand
import com.edawg878.bukkit.commands.PlotCommand.PlotCommand
import com.edawg878.bukkit.commands.TierCommand.TierCommand
import com.edawg878.bukkit.listener.PlotListener
import com.edawg878.bukkit.plot.PlotClearConversation.PlotClearConversation
import com.edawg878.bukkit.plot.{PlotWorld, PlotManager, PlotWorldConfig}
import org.bukkit.World
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

    val tierCommand = new TierCommand(playerDb)
    val perkCommand = new PerkCommand(playerDb)
    val creditCommand = new CreditCommand(playerDb)
    val groupCommand = new GroupCommand(playerDb)
    val playTimeCommand = new PlayTimeCommand(playerDb, server)
    val seenCommand = new SeenCommand(playerDb, server)
    val whoIsCommand = new WhoIsCommand(playerDb, server)
    val plotClearConversation = new PlotClearConversation(getPlotManager, bukkitPlugin, plotDb)
    val plotCommand = new PlotCommand(getPlotManager, playerDb, plotDb, server, bukkitServer, plotClearConversation)

    val commands = Seq[Command[CommandSender]](tierCommand, perkCommand, creditCommand, groupCommand, playTimeCommand,
      seenCommand, whoIsCommand, plotCommand)

    val plotListener = new PlotListener(getPlotManager, plotDb, server, bukkitServer)

    val listeners = Seq[Listener](plotListener)

    val configFile = plugin.dataFolder.resolve("config.json")
    //var config = Configuration.tryLoad[Config](plugin, configFile)
    val plotWorldConfigFile = plugin.dataFolder.resolve("worlds.json")
    //val plotWorldConfig = Configuration.tryLoad(plugin, plotWorldConfigFile)
    val plotWorldConfig = PlotWorldConfig(Seq(PlotWorld(name = "world")))
    val plots = loadPlots


    def plugin: Plugin = bukkitPlugin.toPlugin
    def server: Server = bukkitServer.toServer(bukkitPlugin)
    def bukkitPlugin: org.bukkit.plugin.Plugin
    def bukkitServer: org.bukkit.Server = bukkitPlugin.getServer

    def getPlotManager(bw: World): Option[PlotManager] = plots.get(bw.getName)

    def getPlotWorld(name: String): Option[PlotWorld] = plotWorldConfig.worlds.find(_.name == name)

    def loadPlots: Map[String, PlotManager] = {
      val fMan = plotWorldConfig.worlds
        .map(PlotManager.load(_, plotDb))
      val fManSeq = Future.sequence(fMan)
      val r = Await.result(fManSeq, 5 minutes)
      r.map{ case m => m.w.name -> m }.toMap
    }

  }
}