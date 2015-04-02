package com.edawg878.common

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Conversions {

  implicit class RichTraversable[A](val self: Traversable[A]) extends AnyVal {

    def mkStringPretty: String = self.mkString(MessageFormatter.SECONDARY,
      MessageFormatter.PRIMARY + ", " + MessageFormatter.SECONDARY,
      MessageFormatter.PRIMARY)

    def toOption: Option[Traversable[A]] = {
      if (self.isEmpty) None
      else Some(self)
    }

  }

  implicit class RichInt(val self: Int) extends AnyVal {

    def clamp(min: Int, max: Int): Int = {
      if (self > max) max
      else if (self < min) min
      else self
    }

  }

}