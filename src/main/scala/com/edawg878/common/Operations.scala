package com.edawg878.common

import scopt.Read

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object Operations {

  sealed trait IntOp {
    def using(x: Int, y: Int): Int
  }
  sealed trait PerkOp {
    def using(col: Set[String], str: String): Set[String]
  }
  sealed trait GroupOp {
    def using(a: Group, b: Group): Group
  }

}

object IntOps {

  import Operations.IntOp

  case object Add extends IntOp {
    def using(x: Int, y: Int) = x + y
  }
  case object Subtract extends IntOp {
    def using(x: Int, y: Int) = x - y
  }
  case object Set extends IntOp {
    def using(x: Int, y: Int) = y
  }
  case object Show extends IntOp {
    def using(x: Int, y: Int) = x
  }

}

object PerkOps {

  import Operations.PerkOp

  case object Add extends PerkOp {
    def using(col: Set[String], str: String) = col + str
  }
  case object Subtract extends PerkOp {
    def using(col: Set[String], str: String) = col - str
  }
  case object Show extends PerkOp {
    def using(col: Set[String], str: String) = col
  }
  case object Clear extends PerkOp {
    def using(col: Set[String], str: String) = Set.empty
  }

}

object GroupOps {

  import Operations.GroupOp

  case object Promote extends GroupOp {
    def using(a: Group, b: Group) = Group.promote(a)
  }
  case object Demote extends GroupOp {
    def using(a: Group, b: Group) = Group.demote(a)
  }
  case object Set extends GroupOp {
    def using(a: Group, b: Group) = b
  }
  case object Show extends GroupOp {
    def using(a: Group, b: Group) = a
  }

}

class IllegalOperation extends IllegalArgumentException("Invalid Operation")

trait IntOpsReader {

  import Operations.IntOp
  import IntOps._

  implicit val intOpsReader: Read[IntOp] = Read.reads {
    case "+" => Add
    case "-" => Subtract
    case "set" => Set
    case "show" => Show
    case _ => throw new IllegalOperation
  }

}

trait PerkOpsReader {

  import Operations.PerkOp
  import PerkOps._

  implicit val perkOpsReader: Read[PerkOp] = Read.reads {
      case "+" => Add
      case "-" => Subtract
      case "show" => Show
      case "clear" => Clear
      case _ => throw new IllegalOperation
  }

}

trait GroupOpsReader {

  import Operations.GroupOp
  import GroupOps._

  implicit val groupOpsReader: Read[GroupOp] = Read.reads {
      case "promote" => Promote
      case "demote" => Demote
      case "set" => Set
      case "show" => Show
      case _ => throw new IllegalOperation
  }

}