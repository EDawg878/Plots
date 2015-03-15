package com.edawg878.common

import com.edawg878.bukkit.commands.{PerkCommand, TierCommand, TestCommand}
import com.softwaremill.macwire.Macwire

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Modules {

  trait Module extends Macwire {

    lazy val logger = plugin.getLogger
    lazy val db: MongoPlayerRepository = wire[MongoPlayerRepository]
    lazy val testCommand = wire[TestCommand]
    lazy val tierCommand = wire[TierCommand]
    lazy val perkCommand = wire[PerkCommand]


    def plugin: Plugin

  }

}