package com.edawg878.common

import scopt.Read

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Readers {

  trait GroupReader {

    implicit val groupReader: Read[Group] =
      Read.reads(name => Group.withName(name).getOrElse(throw new IllegalArgumentException(s"Invalid group '$name'")))

  }

  trait PlayerDataReader {

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Future

    implicit val playerDataReader: Read[Future[PlayerData]] =
      Read.reads { name =>
        db.search(name) map { seq =>
          if (seq.isEmpty)
            throw new IllegalArgumentException(s"Player data not found for username '$name'")
          else seq.head
        }
      }

    def db: PlayerRepository

  }

  sealed trait PlayerReader[P] {

    def getPlayer(name: String): P

    implicit val player: Read[P] =
      Read.reads(s => Option(getPlayer(s)).getOrElse(throw new IllegalArgumentException(s"'$s' is not online")))

  }

  object Bukkit {

    import org.bukkit.Server
    import org.bukkit.entity.Player

    trait BukkitReaders extends PlayerReader[Player] {

      override def getPlayer(name: String): Player = bukkitServer.getPlayerExact(name)

      def bukkitServer: Server

    }

  }

  object Bungee {

    import net.md_5.bungee.api.ProxyServer
    import net.md_5.bungee.api.connection.ProxiedPlayer

    trait BungeeReaders extends PlayerReader[ProxiedPlayer] {

      override def getPlayer(name: String): ProxiedPlayer = server.getPlayer(name)

      def server: ProxyServer

    }
  }

}
