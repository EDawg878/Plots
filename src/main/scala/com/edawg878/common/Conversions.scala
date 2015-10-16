package com.edawg878.common

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Conversions {

  implicit class TraversableHelper[A](val self: Traversable[A]) extends AnyVal {

    def mkStringPretty: String =
      self.mkString(Color.Secondary.toString, Color.Primary + ", " + Color.Secondary, Color.Primary.toString)

  }

  implicit class IntHelper(val self: Int) extends AnyVal {

    def clamp(min: Int, max: Int): Int = {
      if (self > max) max
      else if (self < min) min
      else self
    }

  }


}
