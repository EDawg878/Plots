package com.edawg878.common

import java.util.UUID

import com.edawg878.bukkit.plot.PlotClearConversation.PlotClearConversation
import com.edawg878.bukkit.plot.PlotCommand.{DefaultPlotLimitChecker, PlotCommand, PlotLimitChecker}
import com.edawg878.bukkit.plot._
import com.edawg878.common.Server._
import org.bukkit.World
import org.bukkit.command.CommandSender
import org.bukkit.event.Listener
import reactivemongo.api.MongoDriver

import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Modules {

  trait CommonModule {
    val logger = plugin.logger
    val driver = new MongoDriver
    val conn = driver.connection(List("localhost"))
    val mongo = conn.db("minecraft")
    val playerDb = new MongoPlayerRepository(mongo, logger)
    val plotDb = new MongoPlotRepository(mongo, logger)
    val databases = Seq[MongoRepository](playerDb, plotDb)

    def plugin: Plugin
    def server: Server
  }

  trait BukkitModule extends CommonModule with CustomReads {

    import com.edawg878.bukkit.BukkitConversions._

    val configFile = plugin.dataFolder.resolve("vehicles.json")
    val plotWorldConfig = {
      val config = new Configuration[Seq[PlotWorldConfig]](plugin, "worlds.json")
      config.saveDefault()
      config.parse
    }
    val worldEditConfig = {
      val config = new Configuration[WorldEditConfig](plugin, "worldedit.json")
      config.saveDefault()
      config.parse
    }
    val plotWorlds = loadPlotWorlds

    def plugin: Plugin = bukkitPlugin.toPlugin
    def server: Server = bukkitServer.toServer(bukkitPlugin)
    def bukkitPlugin: org.bukkit.plugin.Plugin
    def bukkitServer: org.bukkit.Server = bukkitPlugin.getServer

    val pluginManager: org.bukkit.plugin.PluginManager = bukkitServer.getPluginManager

    val bukkitPlotWorldResolver = new PlotWorldResolver {
      override def apply(s: String): Option[PlotWorld] = plotWorlds.get(s)
    }

    def getPlotWorld(s: String): Option[PlotWorld] = bukkitPlotWorldResolver(s)
    def getPlotWorldConfig(s: String): Option[PlotWorldConfig] = plotWorldConfig.find(_.name == s)

    def setWorldBorder(bw: World): Unit =
      getPlotWorld(bw.getName).foreach(_.setInitialBorder(bw))

    def loadPlotWorlds: Map[String, PlotWorld] = {
      val fMan = plotWorldConfig.map(PlotWorld.load(_, plotDb))
      val fManSeq = Future.sequence(fMan)
      val r = Await.result(fManSeq, 5 minutes)
      r.map{ case w => (w.config.name, w) }.toMap
    }

    def worlds: Seq[UUID] = bukkitServer.getWorlds.map(_.getUID)

    def startTask(t: Schedulable): Task = {
      if (t.async) server.scheduleAsync(t.period, t.delay, t.run)
      else server.schedule(t.period, t.delay, t.run)
    }

    val plotClearConversation = new PlotClearConversation(bukkitPlotWorldResolver, bukkitPlugin, plotDb, bukkitServer)
    val plotListener = new PlotListener(bukkitPlotWorldResolver, plotDb, server, bukkitServer)

    var worldEditListener: Option[WorldEditListener] = None
    var plotLimitChecker: PlotLimitChecker = new DefaultPlotLimitChecker(playerDb)
    val plotCommand = new PlotCommand(bukkitPlotWorldResolver, playerDb, plotDb, server, bukkitServer, plotClearConversation, plotLimitChecker)

    val commands = Seq[Command[CommandSender]](plotCommand)
    val listeners = Seq[Listener](plotListener)

  }
}