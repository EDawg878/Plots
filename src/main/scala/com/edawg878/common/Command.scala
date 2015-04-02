package com.edawg878.common

import scopt.{Read, CustomOptionParser}

import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Command {

  trait ConfigCommand[C, S] extends ErrorConverter[S] {

    val default: C

    val parser: CustomOptionParser[C, S]

    def handle(sender: S, config: C): Unit

    def run(sender: S, args: Array[String]): Boolean = {
      parser.parse(args, default)(sender) match {
        case Some(c) =>
          handle(sender, c)
          true
        case None =>
          false
      }
    }

  }

  trait ErrorConverter[S] {

    def onComplete[A](sender: S, a: Future[A])(f: A => Unit): Unit = {
      a.onComplete {
        case Success(result) =>
          f(result)
        case Failure(t) =>
          parser.reportError(t.getMessage)(sender)
      }
    }

    def parser: CustomOptionParser[_, S]
  }


  type IntOp = (Int, Int) => Int
  type PerkOp = (Set[String], String) => Set[String]

  trait IntOps {

    val Add: IntOp = (x, y) => x + y

    val Subtract: IntOp = (x, y) => x - y

    val Set: IntOp = (_, y) => y

    val Show: IntOp = (x, _) => x

    implicit val reader: Read[IntOp] =
      Read.reads {
        case "+" => Add
        case "-" => Subtract
        case "set" => Set
        case "show" => Show
        case _ => throw new IllegalArgumentException("Invalid Operation")
      }

  }

  trait PerkOps {

    val Add: PerkOp = (col, str) => col + str

    val Subtract: PerkOp = (col, str) => col - str

    val Show: PerkOp = (col, _) => col

    val Clear: PerkOp = (_, _) => Set.empty

    implicit val reader: Read[PerkOp] =
      Read.reads {
        case "+" => Add
        case "-" => Subtract
        case "show" => Show
        case "clear" => Clear
        case _ => throw new IllegalArgumentException("Invalid Operation")
      }

  }

}
