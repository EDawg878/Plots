package com.edawg878.common

import java.util.logging.Logger

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Logging {

  var log: Logger = Logger.getLogger(getClass.getCanonicalName)

  trait PluginLogging {

    protected lazy val logger: Logger = log

  }

}
