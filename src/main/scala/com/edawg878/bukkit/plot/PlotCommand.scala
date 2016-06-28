package com.edawg878.bukkit.plot

import java.time.{Duration, Instant}
import java.util.UUID

import com.edawg878.bukkit.plot.Plot._
import com.edawg878.bukkit.plot.PlotClearConversation.PlotClearConversation
import com.edawg878.common.BukkitCommandHandler.{BukkitCommand, BukkitOptionParser}
import com.edawg878.common.Color.Formatter
import com.edawg878.common.Conversions._
import com.edawg878.common.Readers.Bukkit.BukkitReaders
import com.edawg878.common.Server.Server
import com.edawg878.common.{CommandMeta, PlayerRepository, PlotRepository}
import org.bukkit.ChatColor._
import org.bukkit.command.CommandSender
import org.bukkit.entity.Player

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

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

  trait PlotLimitChecker {

    def getPlotLimit(player: Player): Future[Int]

  }

  class EDawgPlotLimitChecker extends PlotLimitChecker {

    import com.edawg878.core.Core

    def edawg878: Core = Core.getInstance()

    override def getPlotLimit(player: Player): Future[Int] =
      Future.successful(edawg878.getPlayerManager.getData(player).getPlotLimit)
  }

  class DefaultPlotLimitChecker(playerDb: PlayerRepository) extends PlotLimitChecker {

    override def getPlotLimit(player: Player): Future[Int] =
      playerDb.search(player.getUniqueId).map(_.map(_.plotLimit).getOrElse(1))
  }

  class PlotCommand(val resolver: PlotWorldResolver, playerDb: PlayerRepository, plotDb: PlotRepository,
                    val server: Server, val bukkitServer: org.bukkit.Server, plotClearConversation: PlotClearConversation,
                    plotLimitChecker: => PlotLimitChecker)
    extends BukkitCommand[Config] with PlotHelper with BukkitReaders {

    override def meta: CommandMeta = CommandMeta(cmd = "plot", perm = None, aliases = "p", "plotme")

    override val default: Config = Config(sub = None, player = null, target = null, home = "1", id = null)

    override val parser = new BukkitOptionParser[Config]("/plot") {
      cmd("info", Set("i")) action { (_, c) => c.copy(sub = Some(Info))
      } text "display plot information"
      cmd("claim") action { (_, c) => c.copy(sub = Some(Claim))
      } text "claim a free or expired plot"
      cmd("dispose", Set("unclaim")) action { (_, c) => c.copy(sub = Some(Dispose))
      } text "remove ownership of a plot"
      cmd("home", Set("h")) action { (_, c) =>
        c.copy(sub = Some(Home))
      } text "teleport to your plot" children (
        arg[String]("<alias|number>") optional() action { (x, c) => c.copy(home = x) }
        )
      cmd("visit", Set("v")) action { (_, c) =>
        c.copy(sub = Some(Visit))
      } text "visit a player's plot" children(
        arg[String]("<player>") required() action { (x, c) => c.copy(target = x) },
        arg[String]("<alias|number>") optional() action { (x, c) => c.copy(home = x) }
        )
      cmd("add", Set("+")) action { (_, c) =>
        c.copy(sub = Some(Add))
      } text "add a player to the plot" children (
        arg[Player]("<player>") required() action { (x, c) => c.copy(player = x) }
        )
      cmd("trust", Set("++")) action { (_, c) =>
        c.copy(sub = Some(Trust))
      } text "trust a player to the plot" children (
        arg[Player]("<player>") required() action { (x, c) => c.copy(player = x) }
        )
      cmd("remove", Set("-")) action { (_, c) =>
        c.copy(sub = Some(Remove))
      } text "remove a player from the plot" children (
        arg[String]("<player|uuid>") required() action { (x, c) => c.copy(target = x) }
        )
      cmd("ban", Set("deny")) action { (_, c) =>
        c.copy(sub = Some(Ban))
      } text "ban a player from the plot" children (
        arg[Player]("<player>") required() action { (x, c) => c.copy(player = x) }
        )
      cmd("kick") action { (_, c) =>
        c.copy(sub = Some(Kick))
      } text "kick a player from the plot" children (
        arg[Player]("<player>") required() action { (x, c) => c.copy(player = x) }
        )
      cmd("unban") action { (_, c) =>
        c.copy(sub = Some(Unban))
      } text "unban a player from the plot" children (
        arg[String]("<player|uuid>") required() action { (x, c) => c.copy(target = x) }
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
      } text "teleport to plot coordinates" children (
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

    override def handle(sender: CommandSender, c: Config): Unit =
      c.sub collect {
        case Info =>
          for {
            p <- asPlayerOrErr(sender)
            w <- inPlotWorldOrErr(p)
          } yield info(p, w)
        case Claim =>
          for {
            p <- asPlayerOrErr(sender)
            w <- inPlotWorldOrErr(p)
          } yield claim(p, w)
        case Dispose =>
          asPlayerOrErr(sender).foreach(dispose)
        case Home | Visit =>
          for {
            p <- asPlayerOrErr(sender)
            w <- inPlotWorldOrErr(p)
          } yield home(p, w, c)
        case Add =>
          asPlayerOrErr(sender).foreach(add(_, c))
        case Trust =>
          asPlayerOrErr(sender).foreach(trust(_, c))
        case Ban =>
          asPlayerOrErr(sender).foreach(ban(_, c))
        case Kick =>
          asPlayerOrErr(sender).foreach(kick(_, c))
        case Remove =>
          asPlayerOrErr(sender).foreach(remove(_, c))
        case Unban =>
          asPlayerOrErr(sender).foreach(unban(_, c))
        case Open =>
          asPlayerOrErr(sender).foreach(open)
        case Close =>
          asPlayerOrErr(sender).foreach(close)
        case Protect =>
          asPlayerOrErr(sender).foreach(protect)
        case Auto =>
          for {
            p <- asPlayerOrErr(sender)
            w <- inPlotWorldOrErr(p)
          } yield auto(p, w)
        case Clear =>
          asPlayerOrErr(sender).foreach(clear)
        case Reset =>
          asPlayerOrErr(sender).foreach(reset)
        case Teleport =>
          for {
            p <- asPlayerOrErr(sender)
            w <- inPlotWorldOrErr(p)
          } yield teleport(p, w, c)
        case Road =>
          asPlayerOrErr(sender).foreach(road)
      }

    def fmtGroup(ids: Set[UUID], nm: Map[UUID, String]): String =
      if (ids.isEmpty) "none"
      else ids.map(nm).mkStringPretty

    def names(ids: Set[UUID]): Future[Map[UUID, String]] =
      playerDb.traverseById(ids.toSeq)
        .map(_.map(d => (d.id -> d.name)).toMap)
        .map(_.withDefault(_.toString))

    def canClaimPlot(p: Player, pm: PlotWorld): Future[Boolean] = {
      if (p.hasPermission("plot.admin")) Future.successful(true)
      else {
        val homes = pm.getHomes(p.getUniqueId).length
        plotLimitChecker.getPlotLimit(p).map(_ > homes)
      }
    }

    def parseUniqueId(p: Player, s: String)(fn: UUID => Unit): Unit = {
      val f = if (s.length > 16) Future.successful(Try(UUID.fromString(s)).toOption)
      else playerDb.search(s).map(_.map(_.id))
      f.map(_.fold(p.sendMessage(err"Player '$s' could not be found"))(fn))
    }

    def info(p: Player, w: PlotWorld): Unit = {
      val id = w.getPlotId(p.getLocation)
      w.getPlot(id).fold(p.sendMessage(info"Vacant plot ($id)")) { plot =>
        names(plot.ids).foreach { nm =>
          p.sendMessage(info"Plot ID: $id")
          p.sendMessage(info"Owner: ${nm.getOrElse(plot.owner, plot.owner.toString)}")
          p.sendMessage(info"Expiration: ${if (plot.protect) GREEN + "protected" else RED + plot.ExpirationFormatter.format(plot.expirationDate)}")
          p.sendMessage(info"Status: ${if (plot.closed) RED + "closed" else GREEN + "open"}")
          p.sendMessage(info"Road Access ${if (plot.roadAccess) GREEN + "enabled" else RED + "disabled"}")
          p.sendMessage(info"Protected: ${if (plot.protect) GREEN + "yes" else RED + "no"}")
          p.sendMessage(info"Helpers: ${fmtGroup(plot.helpers, nm)}")
          p.sendMessage(info"Trusted: ${fmtGroup(plot.trusted, nm)}")
          p.sendMessage(info"Banned: ${fmtGroup(plot.banned, nm)}")
        }
      }
    }

    def claim(p: Player, w: PlotWorld): Unit = {
      val id = w.getPlotId(p.getLocation)
      val plot = w.getPlot(id)
      val expired = plot.fold(false)(_.isExpired)
      if (plot.isEmpty || expired) {
        canClaimPlot(p, w).onComplete {
          case Success(allowed) =>
            if (allowed) {
              if (expired) server.sync(() => w.clear(p.getWorld, id))
              val plot = w.claim(p, p.getWorld, id)
              plotDb.save(plot)
              p.sendMessage(info"Claimed plot ($id)")
            } else {
              p.sendMessage(err"You have reached your plot limit")
            }
          case Failure(ex) =>
            p.sendMessage(err"Error checking plot limit")
        }
      } else {
        p.sendMessage(err"This plot has already been claimed")
      }
    }

    def dispose(p: Player): Unit =
      withPlotStatusOrErr(p, Admin).fold(p.sendMessage(err"You do not have permission to dispose of the plot")) {
        case (w, plot) =>
          w.unclaim(plot.id)
          plotDb.delete(plot.id)
          p.sendMessage(info"Disposed plot (${plot.id}})")
      }

    def home(p: Player, w: PlotWorld, c: Config): Unit = {
      def teleport(target: UUID): Unit = Try(c.home.toInt).toOption
        .fold(w.getHome(target, c.home))(w.getHome(target, _))
        .fold(p.sendMessage(err"Home '${c.home}' not found")) { plot =>
          p.sendMessage(info"Teleporting to plot (${plot.id})")
          server.sync(() => p.teleport(w.getHomeLocation(p.getWorld, plot.id)))
        }
      c.sub collect {
        case Home => teleport(p.getUniqueId)
        case Visit =>
          val uuid = playerDb.find(c.target).map(_.id)
          uuid.onSuccess { case id => teleport(id) }
          uuid.failed.foreach(sendErr(p, _))
      }
    }

    def add(p: Player, c: Config): Unit =
      withPlotStatusOrErr(p, Owner).fold(p.sendMessage(err"You do not have permission to add players to the plot")) {
        case (w, plot) =>
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

    def trust(p: Player, c: Config): Unit =
      withPlotStatusOrErr(p, Owner).fold(p.sendMessage(err"You do not have permission to trust players to the plot")) {
        case (w, plot) =>
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

    def ban(p: Player, c: Config): Unit =
      withPlotStatusOrErr(p, Owner).fold(p.sendMessage(err"You do not have permission to ban players from the plot")) {
        case (w, plot) =>
          if (c.player.hasPermission("plot.bypass.ban")) {
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

    def kick(p: Player, c: Config): Unit =
      withPlotStatusOrErr(p, Trusted).fold(p.sendMessage(err"You do not have permission to kick players from the plot")) {
        case (w, plot) =>
          if (c.player.hasPermission("plot.bypass.kick")) {
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

    def remove(p: Player, c: Config): Unit =
      withPlotStatusOrErr(p, Owner).fold(p.sendMessage(err"You do not have permission to remove players from the plot")) {
        case (w, plot) =>
          if (c.target == "all") {
            val removed = plot.copy(helpers = Set(), trusted = Set())
            w.update(removed)
            plotDb.save(removed)
            p.sendMessage(info"Removed all players from plot")
          } else {
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

    def unban(p: Player, c: Config): Unit =
      withPlotStatusOrErr(p, Owner).fold(p.sendMessage(err"You do not have permission to unban players from the plot")) {
        case (w, plot) =>
          if (c.target == "all") {
            val removed = plot.copy(banned = Set())
            w.update(removed)
            plotDb.save(removed)
            p.sendMessage(info"Unbanned all players from the plot")
          } else {
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

    def open(p: Player): Unit =
      withPlotStatusOrErr(p, Owner).fold(p.sendMessage(err"You do not have permission to open the plot")) {
        case (w, plot) =>
          if (plot.open) {
            p.sendMessage(err"The plot is already open to visitors")
          } else {
            val opened = plot.copy(closed = false)
            w.update(opened)
            plotDb.save(opened)
            p.sendMessage(info"The plot has been opened to visitors")
          }
      }

    def close(p: Player): Unit =
      withPlotStatusOrErr(p, Owner).fold(p.sendMessage(err"You do not have permission to close the plot")) {
        case (w, plot) =>
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

    def protect(p: Player): Unit =
      withPlotStatusOrErr(p, Admin).fold(p.sendMessage(err"You do not have permission to protect the plot")) {
        case (w, plot) =>
          val toggled = plot.copy(protect = !plot.protect)
          w.update(toggled)
          plotDb.save(toggled)
          if (toggled.protect) p.sendMessage(info"The plot has been protected")
          else p.sendMessage(info"The plot is no longer protected")
      }

    def auto(p: Player, w: PlotWorld): Unit = {
      canClaimPlot(p, w).onComplete {
        case Success(canClaim) =>
          if (canClaim) {
            @tailrec
            def nextId(n: Int, x: Int, z: Int): (PlotId, Boolean) = {
              val id = new PlotId(x, z, w.config.name)
              val plot = w.getPlot(id)
              val expired = plot.fold(false)(_.isExpired)
              if (plot.isEmpty || expired) (id, expired)
              else {
                if (z < n) nextId(n, x, z + 1)
                else if (x < n) nextId(n, x + 1, -n)
                else nextId(n + 1, -(n + 1), -(n + 1))
              }
            }
            nextId(0, 0, 0) match {
              case (id, expired) =>
                // NOTE: Too laggy so automatic clearing has been disabled
                //if (expired) server.sync(()=> w.clear(p.getWorld, id))
                val updated = w.claim(p, p.getWorld, id)
                plotDb.save(updated)
                server.sync(() => {
                  p.teleport(w.getHomeLocation(p.getWorld, id))
                  p.sendMessage(info"Claimed plot ($id)")
                })
            }
          } else {
            p.sendMessage(err"You have reached your plot limit")
          }
        case Failure(ex) =>
          p.sendMessage(err"Error checking plot limit")
      }
    }

    def clear(p: Player): Unit =
      withPlotStatusOrErr(p, Owner).fold(p.sendMessage(err"You do not have permission to clear the plot")) {
        case (w, plot) =>
          if (plot.protect) {
            p.sendMessage(err"You cannot clear a protected plot")
          } else if (p.hasPermission("plot.admin") || plot.canClear(Duration.ofHours(48))) {
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

    def reset(p: Player): Unit =
      withPlotStatusOrErr(p, Admin).fold(p.sendMessage(err"You do not have permission to reset the plot")) {
        case (w, plot) =>
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

    def teleport(p: Player, w: PlotWorld, c: Config): Unit = {
      if (p.hasPermission("plot.admin")) {
        PlotId.parse(w.config, c.id) map { id =>
          val home = w.getHomeLocation(p.getWorld, id)
          p.sendMessage(info"Teleporting to plot ($id)")
          p.teleport(home)
        } getOrElse (p.sendMessage(err"Invalid coordinate format"))
      } else {
        p.sendMessage(err"You do not have permission to teleport to plot coordinates")
      }
    }

    def road(p: Player): Unit =
      withPlotStatusOrErr(p, Admin).fold(p.sendMessage(err"You do not have permission to toggle road access")) {
        case (w, plot) =>
          val toggled = plot.copy(roadAccess = !plot.roadAccess)
          val action = if (toggled.roadAccess) "enabled" else "disabled"
          w.update(toggled)
          plotDb.save(toggled)
          p.sendMessage(info"Road access has been $action")
      }

  }

}
