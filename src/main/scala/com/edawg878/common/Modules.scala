package com.edawg878.common

import com.edawg878.bukkit.commands.Basic.{WhoIsCommand, SeenCommand, PlayTimeCommand}
import com.edawg878.bukkit.commands.Group.GroupCommand
import com.edawg878.bukkit.listener.PlotListener
import com.edawg878.bukkit.plot.{PlotWorld, PlotManager, PlotWorldConfig}
import com.edawg878.common.Server.CustomCombinators._
import org.bukkit.World
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import com.softwaremill.macwire.Macwire
import com.edawg878.common.Server._
import org.bukkit.command.CommandSender
import reactivemongo.api.{DB, MongoDriver}

import scala.concurrent.{Await, Future}

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Modules {

  trait CommonModule extends Macwire {
    lazy val logger = plugin.logger
    lazy val driver = new MongoDriver
    lazy val conn = driver.connection(List("localhost"))
    lazy val mongo: DB = conn.db("minecraft")
    lazy val db = wire[MongoPlayerRepository]
    lazy val plotDb = wire[MongoPlotRepository]

    def plugin: Plugin
    def server: Server
  }

  trait BukkitModule extends CommonModule {

    import com.edawg878.bukkit.BukkitConversions._
    import com.edawg878.bukkit.commands.Credit.CreditCommand
    import com.edawg878.bukkit.commands.Perk.PerkCommand
    import com.edawg878.bukkit.commands.Tier.TierCommand
    import com.edawg878.bukkit.commands.Plot.PlotCommand

    lazy val tierCommand: Command[CommandSender] = wire[TierCommand]
    lazy val perkCommand: Command[CommandSender] = wire[PerkCommand]
    lazy val creditCommand: Command[CommandSender] = wire[CreditCommand]
    lazy val groupCommand: Command[CommandSender] = wire[GroupCommand]
    lazy val playTimeCommand: Command[CommandSender] = wire[PlayTimeCommand]
    lazy val seenCommand: Command[CommandSender] = wire[SeenCommand]
    lazy val whoIsCommand: Command[CommandSender] = wire[WhoIsCommand]
    lazy val plotCommand: Command[CommandSender] = new PlotCommand(getPlotManager, db, plotDb)

    lazy val commands = Seq(tierCommand, perkCommand, creditCommand, groupCommand, playTimeCommand, seenCommand,
      whoIsCommand, plotCommand)

    lazy val plotListener = new PlotListener(getPlotManager, plotDb)

    lazy val listeners = Seq(plotListener)

    val configFile = plugin.dataFolder.resolve("config.json")
    //var config = Configuration.tryLoad[Config](plugin, configFile)
    val plotWorldConfigFile = plugin.dataFolder.resolve("worlds.json")
    //val plotWorldConfig = Configuration.tryLoad(plugin, plotWorldConfigFile)
    val plotWorldConfig = PlotWorldConfig(Seq(PlotWorld(name = "world")))
    val plots = loadPlots


    def plugin: Plugin = bukkitPlugin.toPlugin
    def server: Server = bukkitPlugin.getServer.toServer
    def bukkitPlugin: org.bukkit.plugin.Plugin

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