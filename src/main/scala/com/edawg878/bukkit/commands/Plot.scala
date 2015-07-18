package com.edawg878.bukkit.commands

import java.util.UUID

import com.edawg878.bukkit.plot.{PlotHelper, PlotManager}
import com.edawg878.common.BukkitCommandHandler.{BukkitCommand, BukkitOptionParser}
import com.edawg878.common.{PlotRepository, PlayerRepository, CommandMeta}
import org.bukkit.event.{Cancellable, EventHandler, Listener}
import org.bukkit.event.block.BlockBreakEvent
import org.bukkit.{Location, World}
import org.bukkit.ChatColor._
import org.bukkit.command.CommandSender
import org.bukkit.entity.Player
import com.edawg878.common.Color.Formatter
import com.edawg878.common.Conversions._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Plot {

  sealed trait SubCommand
  case object Info extends SubCommand
  case object Claim extends SubCommand

  case class Config(sub: Option[SubCommand])

  class PlotCommand(val pms: World => Option[PlotManager], playerDb: PlayerRepository, plotDb: PlotRepository) extends BukkitCommand[Config]
    with PlotHelper {

    def meta: CommandMeta = CommandMeta(cmd = "plot", perm = None, aliases = "p", "plotme")

    val default: Config = Config(sub = None)

    val parser = new BukkitOptionParser[Config]("/plot") {
      cmd("info") action { (_, c) => c.copy(sub = Some(Info))
      } text "display plot information"
      cmd("i") action { (_, c) => c.copy(sub = Some(Info)) }
      cmd("claim") action { (_, c) => c.copy(sub = Some(Claim)) }
      checkConfig { c =>
        if (c.sub.isDefined) success else failure("You must specify a subcommand")
      }
    }

    def names(ids: Set[UUID]): Future[Map[UUID, String]] =
      playerDb.traverseById(ids.toSeq: _*)
        .map(_.map(d => (d.id -> d.name)).toMap)
        .map(_.withDefault(_.toString))

    def handle(sender: CommandSender, c: Config): Unit = {
      c.sub collect {
        case Info =>
          asPlayer(sender) { p =>
            inPlotWorld(p) { pm =>
              val id = pm.getPlotId(p.getLocation)
              pm.getPlot(id).map { plot =>
                names(plot.ids).map { nm =>
                  p.sendMessage(info"Plot Id: $id")
                  val owner = nm.getOrElse(plot.owner, plot.owner.toString)
                  p.sendMessage(info"Owner: $owner")
                  val expiration = plot.ExpirationFormatter.format(plot.expirationDate)
                  p.sendMessage(info"Expiration: $expiration")
                  val status = if (plot.closed) RED + "closed" else GREEN + "open"
                  p.sendMessage(info"Status: $status")
                  val roadAccess = if (plot.roadAccess) GREEN + "enabled" else RED + "disabled"
                  p.sendMessage(info"Road Access $roadAccess")
                  val protect = if (plot.protect) GREEN + "yes" else RED + "no"
                  p.sendMessage(info"Protected: $protect")

                  def formatGroup(ids: Set[UUID]): String = {
                    if (ids.isEmpty) "none"
                    else ids.map(nm).mkStringPretty
                  }

                  p.sendMessage(info"Helpers: ${formatGroup(plot.helpers)}")
                  p.sendMessage(info"Trusted: ${formatGroup(plot.trusted)}")
                  p.sendMessage(info"Banned: ${formatGroup(plot.banned)}")
                }
              }.getOrElse(p.sendMessage(info"Vacant plot ($id)"))
            }
          }
        case Claim =>
          asPlayer(sender) { p =>
            inPlotWorld(p) { pm =>
              val id = pm.getPlotId(p.getLocation)
              val plot = pm.getPlot(id)
              val expired = plot.fold(false)(_.isExpired)
              if (plot.isEmpty || expired) {
                if (expired) pm.clear(id)
                val newPlot = pm.claim(p, id)
                plotDb.insert(pm.w, newPlot)
                p.sendMessage(info"Claimed plot ($id)")
              } else {
                p.sendMessage(err"Sorry, this plot has already been claimed")
              }
            }
          }
      }
    }

  }




}