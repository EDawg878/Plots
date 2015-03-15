package com.edawg878.common

import com.softwaremill.macwire.Macwire

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Modules {

  trait Module extends Macwire {

    lazy val logger = plugin.getLogger
    lazy val db: MongoPlayerRepository = wire[MongoPlayerRepository]
    lazy val tierCommand = wire[TierCommand]
    lazy val perkCommand = wire[PerkCommand]
    lazy val creditCommand = wire[CreditCommand]

    def plugin: Plugin

  }

}