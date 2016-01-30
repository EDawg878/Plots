package com.edawg878.bukkit.plot

import java.time.{Instant, Duration}
import java.util.UUID

import com.edawg878.bukkit.plot.Plot._
import com.edawg878.bukkit.plot.PlotClearConversation.PlotClearConversation
import com.edawg878.common.BukkitCommandHandler.{BukkitCommand, BukkitOptionParser}
import com.edawg878.common.Color.Formatter
import com.edawg878.common.Conversions._
import com.edawg878.common.Readers.Bukkit.BukkitReaders
import com.edawg878.common.Server.Server
import com.edawg878.common.{CommandMeta, PlayerRepository, PlotRepository}
import com.edawg878.core.Core
import org.bukkit.ChatColor._
import org.bukkit.command.CommandSender
import org.bukkit.entity.Player

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object PlotCommand {

  sealed trait SubCommand

  case object Info extends SubCommand
  case object Claim extends SubCommand
  case object Home extends SubCommand
  case object Visit extends SubCommand
  case object Add extends SubCommand
  case object Trust extends SubCommand
  case object Remove extends SubCommand
  case object Ban extends SubCommand
  case object Kick extends SubCommand
  case object Unban extends SubCommand
  case object Dispose extends SubCommand
  case object Open extends SubCommand
  case object Close extends SubCommand
  case object Protect extends SubCommand
  case object Auto extends SubCommand
  case object Clear extends SubCommand
  case object Reset extends SubCommand
  case object Teleport extends SubCommand
  case object Road extends SubCommand

  case class Config(sub: Option[SubCommand], player: Player, target: String, home: String, id: String) {
    def pid: UUID = player.getUniqueId
  }

  class PlotCommand(val resolver: PlotWorldResolver, playerDb: PlayerRepository, plotDb: PlotRepository, val server: Server, val bukkitServer: org.bukkit.Server, plotClearConversation: PlotClearConversation,
                     edawg878: => Option[Core]) extends BukkitCommand[Config]
    with PlotHelper with BukkitReaders {

    def meta: CommandMeta = CommandMeta(cmd = "plot", perm = None, aliases = "p", "plotme")

    val default: Config = Config(sub = None, player = null, target = null, home = "1", id = null)

    val parser = new BukkitOptionParser[Config]("/plot") {
      cmd("info", Set("i")) action { (_, c) => c.copy(sub = Some(Info))
      } text "display plot information"
      cmd("claim") action { (_, c) => c.copy(sub = Some(Claim))
      } text "claim a free or expired plot"
      cmd("dispose", Set("unclaim")) action { (_, c) => c.copy(sub = Some(Dispose))
      } text "remove ownership of a plot"
      cmd("home", Set("h")) action { (_, c) =>
        c.copy(sub = Some(Home))
      } text "teleport to your plot" children (
        arg[String]("<alias|number>") optional() action { (x, c) => c.copy(home = x)}
      )
      cmd("visit", Set("v")) action { (_, c) =>
        c.copy(sub = Some(Visit))
      } text "visit a player's plot" children(
        arg[String]("<player>") required() action { (x, c) => c.copy(target = x)},
        arg[String]("<alias|number>") optional() action { (x, c) => c.copy(home = x)}
      )
      cmd("add", Set("+")) action { (_, c) =>
        c.copy(sub = Some(Add))
      } text "add a player to the plot" children (
        arg[Player]("<player>") required() action { (x, c) => c.copy(player = x)}
      )
      cmd("trust", Set("++")) action { (_, c) =>
        c.copy(sub = Some(Trust))
      } text "trust a player to the plot" children (
        arg[Player]("<player>") required() action { (x, c) => c.copy(player = x)}
      )
      cmd("remove", Set("-")) action { (_, c) =>
        c.copy(sub = Some(Remove))
      } text "remove a player from the plot" children (
        arg[String]("<player|uuid>") required() action { (x, c) => c.copy(target = x)}
      )
      cmd("ban", Set("deny")) action { (_, c) =>
        c.copy(sub = Some(Ban))
      } text "ban a player from the plot" children (
        arg[Player]("<player>") required() action { (x, c) => c.copy(player = x)}
      )
      cmd("kick") action { (_, c) =>
        c.copy(sub = Some(Kick))
      } text "kick a player from the plot" children (
        arg[Player]("<player>") required() action { (x, c) => c.copy(player = x)}
      )
      cmd("unban") action { (_, c) =>
        c.copy(sub = Some(Unban))
      } text "unban a player from the plot" children (
        arg[String]("<player|uuid>") required() action { (x, c) => c.copy(target = x)}
      )
      cmd("open") action { (_, c) =>
        c.copy(sub = Some(Open))
      } text "open the plot to visitors"
      cmd("close") action { (_, c) =>
        c.copy(sub = Some(Close))
      } text "close the plot to visitors"
      cmd("protect") action { (_, c) =>
        c.copy(sub = Some(Protect))
      } text "protect the plot from expiration and prevent it from being cleared"
      cmd("auto") action { (_, c) =>
        c.copy(sub = Some(Auto))
      } text "claim the next available plot"
      cmd("clear") action { (_, c) =>
        c.copy(sub = Some(Clear))
      } text "regenerate the plot"
      cmd("reset") action { (_, c) =>
        c.copy(sub = Some(Reset))
      } text "regenerate the plot and dispose of it"
      cmd("teleport", Set("tp")) action { (_, c) =>
        c.copy(sub = Some(Teleport))
      } text "teleport to plot coordinates" children(
        arg[String]("<id>") required() action { (x, c) =>
          c.copy(id = x)
        } text "plot coordinates x;z"
      )
      cmd("road") action { (_, c) =>
        c.copy(sub = Some(Road))
      } text "toggle road access"
      checkConfig { c =>
        if (c.sub.isDefined) success else failure("You must specify a subcommand")
      }
    }

    def fmtGroup(ids: Set[UUID], nm: Map[UUID, String]): String =
      if (ids.isEmpty) "none"
      else ids.map(nm).mkStringPretty

    def names(ids: Set[UUID]): Future[Map[UUID, String]] =
      playerDb.traverseById(ids.toSeq)
        .map(_.map(d => (d.id -> d.name)).toMap)
        .map(_.withDefault(_.toString))

    def getPlotLimit(p: Player): Int =
      edawg878.map(_.getPlayerManager.getData(p).getPlotLimit).getOrElse(1)

    def canClaimPlot(p: Player, pm: PlotWorld): Boolean = {
      if (p.hasPermission("plot.admin")) true
      else {
        val homes = pm.getHomes(p.getUniqueId).length
        getPlotLimit(p) > homes
      }
      //else playerDb.search(p.getUniqueId).map(_.exists(_.plotLimit > pm.getHomes(p.getUniqueId).length))
    }

    def parseUniqueId(p: Player, s: String)(fn: UUID => Unit): Unit = {
      val f = if (s.length > 16) Future(Try(UUID.fromString(s)).toOption)
      else playerDb.search(s).map(_.map(_.id))
      f.map(_.fold(p.sendMessage(err"Player '$s' could not be found"))(fn))
    }

    def handle(sender: CommandSender, c: Config): Unit = {
      c.sub collect {
        case Info =>
          asPlayer(sender)(p =>
            inPlotWorld(p){ w =>
              val id = w.getPlotId(p.getLocation)
              w.getPlot(id).fold(p.sendMessage(info"Vacant plot ($id)")) { plot =>
                names(plot.ids).foreach { nm =>
                  p.sendMessage(info"Plot ID: $id")
                  p.sendMessage(info"Owner: ${nm.getOrElse(plot.owner, plot.owner.toString)}")
                  p.sendMessage(info"Expiration: ${plot.ExpirationFormatter.format(plot.expirationDate)}")
                  p.sendMessage(info"Status: ${if (plot.closed) RED + "closed" else GREEN + "open"}")
                  p.sendMessage(info"Road Access ${if (plot.roadAccess) GREEN + "enabled" else RED + "disabled"}")
                  p.sendMessage(info"Protected: ${if (plot.protect) GREEN + "yes" else RED + "no"}")
                  p.sendMessage(info"Helpers: ${fmtGroup(plot.helpers, nm)}")
                  p.sendMessage(info"Trusted: ${fmtGroup(plot.trusted, nm)}")
                  p.sendMessage(info"Banned: ${fmtGroup(plot.banned, nm)}")
                }
              }
            }
          )
        case Claim =>
          asPlayer(sender) { p =>
            inPlotWorld(p) { w =>
              val id = w.getPlotId(p.getLocation)
              val plot = w.getPlot(id)
              val expired = plot.fold(false)(_.isExpired)
              if (plot.isEmpty || expired) {
                  if (canClaimPlot(p, w)) {
                    if (expired) server.sync(()=> w.clear(p.getWorld, id))
                    val plot = w.claim(p, p.getWorld, id)
                    plotDb.save(plot)
                    p.sendMessage(info"Claimed plot ($id)")
                  } else {
                    p.sendMessage(err"You have reached your maximum number of plots")
                  }
              } else {
                p.sendMessage(err"This plot has already been claimed")
              }
            }
          }
        case Dispose =>
          asPlayer(sender) { p =>
            withPlotStatus(p, Admin, _.sendMessage(err"You do not have permission to dispose of the plot")) { (w, plot) =>
              w.unclaim(plot.id)
              plotDb.delete(plot.id)
              p.sendMessage(info"Disposed plot (${plot.id}})")
            }
          }
        case Home | Visit =>
          asPlayer(sender) { p =>
            inPlotWorld(p) { w =>
              def home(pid: UUID): Unit = Try(c.home.toInt).toOption
                .fold(w.getHome(pid, c.home))(w.getHome(pid, _))
                .fold(p.sendMessage(err"Home '${c.home}' not found")){ plot =>
                  p.sendMessage(info"Teleporting to plot (${plot.id})")
                  server.sync(()=> p.teleport(w.getHomeLocation(p.getWorld, plot.id)))
                }
              c.sub collect {
                case Home => home(p.getUniqueId)
                case Visit => onComplete(sender, playerDb.find(c.target).map(_.id))(home)
              }
            }
          }
        case Add =>
          asPlayer(sender) { p =>
            withPlotStatus(p, Owner, _.sendMessage(err"You do not have permission to add players to the plot")) { (w, plot) =>
              if (plot.isOwner(c.pid)) {
                p.sendMessage(err"You cannot add yourself to the plot")
              } else if (plot.isHelper(c.pid)) {
                p.sendMessage(err"${c.player.getName} is already added to the plot")
              } else {
                val added = plot.copy(helpers = plot.helpers + c.pid)
                w.update(added)
                plotDb.save(added)
                p.sendMessage(info"Added ${c.player.getName} to the plot")
              }
            }
          }
        case Trust =>
          asPlayer(sender) { p =>
            withPlotStatus(p, Owner, _.sendMessage(err"You do not have permission to trust players to the plot")) { (w, plot) =>
              if (plot.isOwner(c.pid)) {
                p.sendMessage(err"You cannot trust yourself to the plot")
              } else if (plot.isTrusted(c.pid)) {
                p.sendMessage(err"${c.player.getName} is already added to the plot")
              } else {
                val trusted = plot.copy(trusted = plot.trusted + c.pid, helpers = plot.helpers - c.pid)
                w.update(trusted)
                plotDb.save(trusted)
                p.sendMessage(info"Trusted ${c.player.getName} to the plot")
              }
            }
          }
        case Ban =>
          asPlayer(sender) { p =>
            withPlotStatus(p, Owner, _.sendMessage(err"You do not have permission to ban players from the plot")) { (w, plot) =>
                if (c.player.hasPermission("plot.ban.bypass")) {
                  p.sendMessage(err"${c.player.getName} cannot be banned from the plot")
                } else if (p.getUniqueId == c.pid) {
                  p.sendMessage(err"You cannot ban yourself from the plot")
                } else if (plot.isOwner(c.pid)) {
                  p.sendMessage(err"You cannot ban the plot owner")
                } else if (plot.isBanned(c.pid)) {
                  p.sendMessage(err"${c.player.getName} is already banned from the plot")
                } else if (w.config.isSpawnPlot(p.getWorld, plot.id)) {
                  p.sendMessage(err"You cannot ban somebody from the spawn")
                } else {
                  val banned = plot.copy(banned = plot.banned + c.pid, helpers = plot.helpers - c.pid, trusted = plot.trusted - c.pid)
                  w.update(banned)
                  plotDb.save(banned)
                  val region = w.config.outer(plot.id)
                  if (region.isInside(c.player.getLocation)) c.player.teleport(c.player.getWorld.getSpawnLocation)
                  p.sendMessage(info"Banned ${c.player.getName} from the plot")
              }
            }
          }
        case Kick =>
          asPlayer(sender) { p =>
            withPlotStatus(p, Trusted, _.sendMessage(err"You do not have permission to kick players from the plot")) { (w, plot) =>
              if (c.player.hasPermission("plot.kick.bypass")) {
                p.sendMessage(err"${c.player.getName} cannot be kicked from the plot")
              } else if (p.getUniqueId == c.pid) {
                p.sendMessage(err"You cannot kick yourself from the plot")
              } else if (plot.isOwner(c.pid)) {
                p.sendMessage(err"You cannot kick the plot owner")
              } else if (plot.isTrusted(c.pid)) {
                p.sendMessage(err"You cannot kick a player who is trusted to the plot")
              } else if (w.config.isSpawnPlot(p.getWorld, plot.id)) {
                p.sendMessage(err"You cannot kick somebody from the spawn")
              } else if (w.config.outer(plot.id).isInside(c.player.getLocation)) {
                c.player.teleport(c.player.getWorld.getSpawnLocation)
                p.sendMessage(info"${c.player.getName} has been kicked from the plot")
                // TODO broadcast kick message to local channel
                c.player.sendMessage(err"You have been kicked from the plot by ${p.getName}")
              } else {
                p.sendMessage(err"${c.player.getName} is not inside the plot")
              }
            }
          }
        case Remove =>
          asPlayer(sender) { p =>
            withPlotStatus(p, Owner, _.sendMessage(err"You do not have permission to remove players from the plot")) { (w, plot) =>
              parseUniqueId(p, c.target) { pid =>
                if (plot.isAdded(pid)) {
                  val removed = plot.copy(helpers = plot.helpers - pid, trusted = plot.trusted - pid)
                  w.update(removed)
                  plotDb.save(removed)
                  p.sendMessage(info"Removed ${c.target} from the plot")
                } else {
                  p.sendMessage(err"${c.target} is not added to the plot")
                }
              }
            }
          }
        case Unban =>
          asPlayer(sender) { p =>
            withPlotStatus(p, Owner, _.sendMessage(err"You do not have permission to unban players from the plot")) { (w, plot) =>
              parseUniqueId(p, c.target) { pid =>
                if (plot.isBanned(pid)) {
                  val removed = plot.copy(banned = plot.banned - pid)
                  w.update(removed)
                  plotDb.save(removed)
                  p.sendMessage(info"Unbanned ${c.target} from the plot")
                } else {
                  p.sendMessage(err"${c.target} is not banned from the plot")
                }
              }
            }
          }
        case Open =>
          asPlayer(sender) { p =>
            withPlotStatus(p, Owner, _.sendMessage(err"You do not have permission to open the plot")) { (w, plot) =>
              if (plot.open) {
                p.sendMessage(err"The plot is already open to visitors")
              } else {
                val opened = plot.copy(closed = false)
                w.update(opened)
                plotDb.save(opened)
                p.sendMessage(info"The plot has been opened to visitors")
              }
            }
          }
        case Close =>
          asPlayer(sender) { p =>
            withPlotStatus(p, Owner, _.sendMessage(err"You do not have permission to close the plot")) { (w, plot) =>
              if (plot.closed) {
                p.sendMessage(err"The plot is already closed to visitors")
              } else if (w.config.isSpawnPlot(p.getWorld, plot.id)) {
                p.sendMessage(err"You cannot close the spawn")
              } else {
                val closed = plot.copy(closed = true)
                w.update(closed)
                plotDb.save(closed)
                p.sendMessage(info"The plot has been closed to visitors")
              }
            }
          }
        case Protect =>
          asPlayer(sender) { p =>
            withPlotStatus(p, Admin, _.sendMessage(err"You do not have permission to protect the plot")) { (w, plot) =>
              val toggled = plot.copy(protect = !plot.protect)
              w.update(toggled)
              plotDb.save(toggled)
              if (toggled.protect) p.sendMessage(info"The plot has been protected")
              else p.sendMessage(info"The plot is no longer protected")
            }
          }
        case Auto =>
          asPlayer(sender) { p =>
            inPlotWorld(p) { w =>
                if (canClaimPlot(p, w) ) {
                  @tailrec
                  def auto(n: Int, x: Int, z: Int): (PlotId, Boolean) = {
                    val id = new PlotId(x, z, w.config.name)
                    val plot = w.getPlot(id)
                    val expired = plot.fold(false)(_.isExpired)
                    if (plot.isEmpty || expired) (id, expired)
                    else {
                      if (z < n) auto(n, x, z + 1)
                      else if (x < n) auto(n, x + 1, -n)
                      else auto(n + 1, -(n + 1), -(n + 1))
                    }
                  }
                  auto(0, 0, 0) match {
                    case (id, expired) =>
                      if (expired) server.sync(()=> w.clear(p.getWorld, id))
                      val updated = w.claim(p, p.getWorld, id)
                      plotDb.save(updated)
                      server.sync(()=> {
                          p.teleport(w.getHomeLocation(p.getWorld, id))
                          p.sendMessage(info"Claimed plot ($id)")
                        })
                  }
                } else {
                  p.sendMessage(err"You cannot claim anymore plots")
                }
            }
          }
        case Clear =>
          asPlayer(sender) { p =>
            withPlotStatus(p, Owner, _.sendMessage(err"You do not have permission to clear the plot")) { (w, plot) =>
              if (plot.protect) {
                p.sendMessage(err"You cannot clear a protected plot")
              } else if (p.hasPermission("plot.admin") || plot.canClear(Duration.ofHours(1))) {
                //plotClearConversation.begin(p, plot.id)
                val id = plot.id
                w.clear(p.getWorld, id)
                val updated = plot.copy(lastCleared = Some(Instant.now))
                w.update(updated)
                plotDb.save(updated)
                p.sendMessage(info"Plot cleared")
              } else {
                p.sendMessage(err"You can only clear your plot once per hour")
              }
            }
          }
        case Reset =>
          asPlayer(sender) { p =>
            withPlotStatus(p, Admin, _.sendMessage(err"You do not have permission to reset the plot")) { (w, plot) =>
              if (plot.protect) {
                p.sendMessage(err"You cannot reset a protected plot")
              } else {
                val id = plot.id
                w.unclaim(id)
                plotDb.delete(id)
                w.clear(p.getWorld, id)
                p.sendMessage(info"Reset plot ($id)")
              }
            }
          }
        case Teleport =>
          asPlayer(sender)(p =>
            inPlotWorld(p) { w =>
              if (p.hasPermission("plot.admin")) {
                PlotId.parse(w.config, c.id) map { id =>
                  val home = w.getHomeLocation(p.getWorld, id)
                  p.sendMessage(info"Teleporting to plot ($id)")
                  p.teleport(home)
                } getOrElse(p.sendMessage(err"Invalid coordinate format"))
              } else {
                p.sendMessage(err"You do not have permission to teleport to plot coordinates")
              }
            }
          )
        case Road =>
          asPlayer(sender)(p =>
            withPlotStatus(p, Admin, _.sendMessage(err"You do not have permission to toggle road access")) { (w, plot) =>
              val toggled = plot.copy(roadAccess = !plot.roadAccess)
              val action = if (toggled.roadAccess) "enabled" else "disabled"
              w.update(toggled)
              plotDb.save(toggled)
              p.sendMessage(info"Road access has been $action")
            }
          )
      }
    }

  }

}

