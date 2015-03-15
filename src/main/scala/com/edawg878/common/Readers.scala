package com.edawg878.common

import scopt.Read

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
trait Readers {

  val db: PlayerRepository

  implicit val playerDataReader: Read[Future[PlayerData]] =
    Read.reads { name =>
      db.search(name) map { seq =>
        if (seq.isEmpty)
          throw new IllegalArgumentException(s"Player data not found for username '$name'")
        else seq.head
      }
    }

}
